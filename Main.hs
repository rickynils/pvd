module Main (
  main,
  scale
) where

import Codec.Image.DevIL
import Control.Concurrent (threadDelay)
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Array.MArray
import Data.Array.Storable
import Data.Bits
import Debug.Trace
import Foreign.C.Types
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Exit (exitWith, ExitCode(..))

data State = State {
  dpy  :: Display,
  win  :: Window,
  img  :: UArray (Int,Int,Int) Word8,
  ximg :: Image,
  sc   :: Scale,
  xoff :: Int,
  yoff :: Int
}

initState :: IO State
initState = do
  dpy' <- openDisplay ""
  rootw <- rootWindow dpy' (defaultScreen dpy')
  win' <- mkWin dpy' rootw
  selectInput dpy' win' (exposureMask .|. buttonPressMask .|. keyPressMask)
  mapWindow dpy' win'
  ilInit
  img' <- readImage "/srv/photo/wrk/export/rhododendron.tif"
  let sc' = (1,5)
  ximg' <- mkImg dpy' img' sc'
  return State { dpy = dpy', win = win', img = img', ximg = ximg', sc = sc', xoff = 0, yoff = 0 }

main :: IO ()
main = initState >>= updateWin

updateWin s = do
  drawInWin (dpy s) (win s) (img s) (ximg s) (xoff s) (yoff s)
  sync (dpy s) True
  allocaXEvent $ \e -> do
    nextEvent (dpy s) e
    ev <- getEvent e
    handleEvent $ ev_event_type ev
  where
    handleEvent ev | ev == buttonPress = return ()
                   | ev == keyPress = updateWin s { xoff = xoff s + 1 }
                   | otherwise = updateWin s


--redraw xo yo sc img = do
--  let (w,h,_) = (snd . bounds) img
--  let (w',h') = (div w 2, div h 2)
--  let bounds' = (0, w'*h')
--  imgArr

s = (1,1)


mkWin dpy rootw = do
  col <- initColor dpy "#444444"
  createSimpleWindow dpy rootw 0 0 100 100 1 col col

drawInWin ::
  Display ->
  Window ->
  UArray (Int,Int,Int) Word8 ->
  Image ->
  Int ->
  Int ->
  IO ()
drawInWin dpy win imgData img x y = do
  bgcolor <- initColor dpy "#444444"
  gc <- createGC dpy win
  (_,_,_,winWidth,winHeight,_,_) <- getGeometry dpy win
  let depth = defaultDepthOfScreen (defaultScreenOfDisplay dpy)
      vis = defaultVisual dpy (defaultScreen dpy)
      (imgHeight,imgWidth,_) = snd $ bounds imgData
      (viewWidth,viewHeight) = scale s (imgWidth,imgHeight)
      portWidth = min (winWidth `div` 2) viewWidth
      portHeight = min (winHeight `div` 2) viewHeight
      portX = (winWidth-portWidth) `div` 2
      portY = (winHeight-portHeight) `div` 2
      viewX = fromIntegral $ min (max 0 (fromIntegral x)) (viewWidth-portWidth)
      viewY = fromIntegral $ min (max 0 (fromIntegral y)) (viewHeight-portHeight)
  pixmap <- createPixmap dpy win winWidth winHeight depth
  setForeground dpy gc bgcolor
  fillRectangle dpy pixmap gc 0 0 winWidth winHeight
  putImage dpy pixmap gc img viewX viewY (fromIntegral portX) (fromIntegral portY) portWidth portHeight
  copyArea dpy pixmap win gc 0 0 winWidth winHeight 0 0
  freeGC dpy gc
  freePixmap dpy pixmap

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros,real) <- allocNamedColor dpy colormap color
  return $ color_pixel apros

mkImg ::
  Display ->
  UArray (Int,Int,Int) Word8 ->
  Scale ->
  IO Image
mkImg dpy img sc = do
  byteArr <- newListArray (0,byteCount) bytes
  withStorableArray byteArr $ \ptr ->
    createImage dpy vis depth zPixmap 0 ptr (fromIntegral w) (fromIntegral h) 32 0

    where
      depth = defaultDepthOfScreen (defaultScreenOfDisplay dpy)
      vis = defaultVisual dpy (defaultScreen dpy)
      (ih,iw,_) = snd $ bounds img
      (h,w) = scale sc (ih,iw)
      byteCount = (4*h*w) - 1
      bytes = map fromIntegral $ elems $ ixmap (0,byteCount) f img
      f n = (ih-r', iw-c', l)
        where
          (c',r') = scale (snd sc, fst sc) (c,r)
          c = (n `div` 4) `mod` w
          r = (n `div` 4) `div` w
          l = case n `mod` 4 of
                0 -> 2
                2 -> 0
                m -> m

scale (s1,s2) (h,w) = (h',w')
  where
    h' = round $ fromIntegral (s1*h) / fromIntegral s2
    w' = round $ fromIntegral (s1*w) / fromIntegral s2


type Scale = (Int,Int)
type PixelPos = (Int,Int,Int)
type Transformer = Scale -> PixelPos -> [(PixelPos,Int)]

