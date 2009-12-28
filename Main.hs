module Main where
import Data.Bits
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent (threadDelay)
import Data.Array.Storable
import Data.Array.MArray
import Foreign.C.Types

main :: IO ()
main = do
  dpy <- openDisplay ""
  rootw <- rootWindow dpy (defaultScreen dpy)
  win <- mkWin dpy rootw
  selectInput dpy win (exposureMask .|. buttonPressMask)
  mapWindow dpy win
  updateWin dpy win

updateWin :: Display -> Window -> IO ()
updateWin dpy win = do
  drawInWin dpy win
  sync dpy True
  allocaXEvent $ \e -> do
    nextEvent dpy e
    ev <- getEvent e
    putStrLn $ eventName ev
    if ev_event_type ev /= buttonPress then updateWin dpy win else return ()

mkWin dpy rootw = do
  col <- initColor dpy "#444444"
  createSimpleWindow dpy rootw 0 0 100 100 1 col col

drawInWin :: Display -> Window -> IO ()
drawInWin dpy win = do
  bgcolor <- initColor dpy "#444444"
  gc <- createGC dpy win
  (_,_,_,w,h,_,_) <- getGeometry dpy win
  let depth = defaultDepthOfScreen (defaultScreenOfDisplay dpy)
      vis = defaultVisual dpy (defaultScreen dpy)
      iw = w `div` 2
      ih = h `div` 2
  p <- createPixmap dpy win w h depth
  setForeground dpy gc bgcolor
  fillRectangle dpy p gc 0 0 w h
  img <- mkImg dpy vis iw ih depth
  putImage dpy p gc img 0 0 (fromIntegral $ (w-iw) `div` 2) (fromIntegral $ (h-ih) `div` 2) iw ih
  copyArea dpy p win gc 0 0 w h 0 0
  freeGC dpy gc
  freePixmap dpy p

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros,real) <- allocNamedColor dpy colormap color
  return $ color_pixel apros

mkImg d v w h depth = do
  let bounds = (0, (4 * (fromIntegral h) * (fromIntegral w)) - 1)
  --let bounds = (0, 3 * (fromIntegral h-1) * (fromIntegral w-1))
  ar <- newArray bounds 50 :: IO (StorableArray Int CChar)
  writeArray ar 0 255
  writeArray ar 1 255
  writeArray ar 2 255
  writeArray ar 3 255
  writeArray ar 4 128
  writeArray ar 5 128
  writeArray ar 6 128
  writeArray ar 7 128
  writeArray ar (4*10*(fromIntegral w) + 4*10) 255
  --writeArray ar (4) 255
  withStorableArray ar $ \ptr ->
    createImage d v depth zPixmap 0 ptr w h 32 0
