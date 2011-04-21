module XUtils (
  initX,
  drawImg,
  loadXImg,
  XImg
) where

import Control.Monad.Error
import Data.Array.Storable
import Data.Foldable (for_)
import Data.Word (Word8)
import Foreign hiding (newArray)
import qualified Codec.Image.DevIL.Extras as IL
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as X

data XImg = XImg {
  xImg :: X.Image,
  xImgData :: StorableArray (Int,Int,Int) Word8,
  xImgH :: Int,
  xImgW :: Int
}

initX = do
  X.initThreads
  dpy <- X.openDisplay ""
  rootw <- X.rootWindow dpy (X.defaultScreen dpy)
  win <- mkWin dpy rootw
  X.selectInput dpy win X.exposureMask
  X.mapWindow dpy win
  X.flush dpy
  return (dpy,win)
  where
    mkWin dpy rootw = do
      attr <- X.getWindowAttributes dpy rootw
      col <- initColor dpy "#000000"
      X.createSimpleWindow dpy rootw 0 0 (fromIntegral $ X.wa_width attr) (fromIntegral $ X.wa_height attr) 1 col col

drawImg :: X.Display -> X.Window -> XImg -> IO ()
drawImg dpy win ximg = do
  bgcolor <- initColor dpy "#000000"
  gc <- X.createGC dpy win
  (_,_,_,winWidth,winHeight,_,_) <- X.getGeometry dpy win
  let depth = X.defaultDepthOfScreen (X.defaultScreenOfDisplay dpy)
  pixmap <- X.createPixmap dpy win winWidth winHeight depth
  X.setForeground dpy gc bgcolor
  X.fillRectangle dpy pixmap gc 0 0 winWidth winHeight
  let portWidth = fromIntegral (xImgW ximg)
      portHeight = fromIntegral (xImgH ximg)
      portX = fromIntegral ((winWidth-portWidth) `div` 2)
      portY = fromIntegral ((winHeight-portHeight) `div` 2)
  X.putImage dpy pixmap gc (xImg ximg) 0 0 portX portY portWidth portHeight
  X.copyArea dpy pixmap win gc 0 0 winWidth winHeight 0 0
  X.freePixmap dpy pixmap
  X.freeGC dpy gc

initColor :: X.Display -> String -> IO X.Pixel
initColor dpy color = do
  let colormap = X.defaultColormap dpy (X.defaultScreen dpy)
  (apros,real) <- X.allocNamedColor dpy colormap color
  return $ X.color_pixel apros

makeXImage :: X.Display -> IL.Image -> IO XImg
makeXImage dpy img = do
  xImgData <- mapIndices bs mapIdx (IL.imgData img)
  withStorableArray xImgData (ci xImgData . castPtr)
  where
    w = fromIntegral (IL.imgWidth img)
    h = fromIntegral (IL.imgHeight img)
    ci imgdata p = do
      ximg <- X.createImage dpy vis depth X.zPixmap 0 p w h 32 0
      return XImg { xImgData = imgdata, xImg = ximg, xImgH = IL.imgHeight img,
        xImgW = IL.imgWidth img }
    depth = X.defaultDepthOfScreen (X.defaultScreenOfDisplay dpy)
    vis = X.defaultVisual dpy (X.defaultScreen dpy)
    bs = ((0,0,0), (IL.imgHeight img - 1, IL.imgWidth img - 1, 3))
    mapIdx (y,x,c) = (IL.imgHeight img - y - 1, x, c' c)
      where
        c' 0 = 2
        c' 1 = 1
        c' 2 = 0
        c' 3 = 0

loadXImg :: X.Display -> String -> IO (Maybe XImg)
loadXImg dpy path = do
  img <- IL.loadImage path
  flip (maybe (return Nothing)) img $ \i -> do
    ximg <- makeXImage dpy i
    IL.unloadImage i
    return $ Just ximg
