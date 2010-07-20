module XUtils (
  initX,
  drawImg,
  sendExposeEvent,
  loadXImg
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
  xImgH :: Int,
  xImgW :: Int
}

initX = do
  dpy <- X.openDisplay ""
  rootw <- X.rootWindow dpy (X.defaultScreen dpy)
  win <- mkWin dpy rootw
  X.selectInput dpy win X.exposureMask
  X.mapWindow dpy win
  return (dpy,win)
  where
    mkWin dpy rootw = do
      col <- initColor dpy "#000000"
      X.createSimpleWindow dpy rootw 0 0 100 100 1 col col

drawImg :: X.Display -> X.Window -> Maybe XImg -> IO ()
drawImg dpy win ximg = do
  bgcolor <- initColor dpy "#444444"
  gc <- X.createGC dpy win
  (_,_,_,winWidth,winHeight,_,_) <- X.getGeometry dpy win
  let depth = X.defaultDepthOfScreen (X.defaultScreenOfDisplay dpy)
  pixmap <- X.createPixmap dpy win winWidth winHeight depth
  X.setForeground dpy gc bgcolor
  X.fillRectangle dpy pixmap gc 0 0 winWidth winHeight
  for_ ximg $ \xi -> do
    let portWidth = fromIntegral (xImgW xi)
        portHeight = fromIntegral (xImgH xi)
        portX = fromIntegral ((winWidth-portWidth) `div` 2)
        portY = fromIntegral ((winHeight-portHeight) `div` 2)
    X.putImage dpy pixmap gc (xImg xi) 0 0 portX portY portWidth portHeight
  X.copyArea dpy pixmap win gc 0 0 winWidth winHeight 0 0
  X.freeGC dpy gc
  X.freePixmap dpy pixmap
  X.sync dpy True

initColor :: X.Display -> String -> IO X.Pixel
initColor dpy color = do
  let colormap = X.defaultColormap dpy (X.defaultScreen dpy)
  (apros,real) <- X.allocNamedColor dpy colormap color
  return $ X.color_pixel apros

makeXImage :: X.Display -> IL.Image -> IO XImg
makeXImage dpy img = do
  xImgData <- mapIndices bs mapIdx (IL.imgData img)
  withStorableArray xImgData (ci . castPtr)
  where
    w = fromIntegral (IL.imgWidth img)
    h = fromIntegral (IL.imgHeight img)
    ci p = do
      ximg <- X.createImage dpy vis depth X.zPixmap 0 p w h 32 0
      return XImg { xImg = ximg, xImgH = IL.imgHeight img, xImgW = IL.imgWidth img }
    depth = X.defaultDepthOfScreen (X.defaultScreenOfDisplay dpy)
    vis = X.defaultVisual dpy (X.defaultScreen dpy)
    bs = ((0,0,0), (IL.imgHeight img - 1, IL.imgWidth img - 1, 3))
    mapIdx (y,x,c) = (IL.imgHeight img - y - 1, IL.imgWidth img - x - 1, c' c)
      where
        c' 0 = 2
        c' 1 = 1
        c' 2 = 0
        c' 3 = 0

loadXImg :: X.Display -> String -> ErrorT String IO XImg
loadXImg dpy path = do
  img <- IL.loadImage path
  ximg <- lift $ makeXImage dpy img
  lift $ IL.unloadImage img
  return ximg

sendExposeEvent dpy win = X.allocaXEvent $ \e -> do
  X.setEventType e X.expose
  X.sendEvent dpy win False X.noEventMask e
  X.flush dpy
