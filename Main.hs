module Main (
  main
) where

import Codec.Image.DevIL
import Control.Concurrent (threadDelay)
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Array.MArray
import Data.Array.Storable
import Data.Bits
import Foreign.C.Types
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Exit (exitWith, ExitCode(..))

main :: IO ()
main = do
  dpy <- openDisplay ""
  rootw <- rootWindow dpy (defaultScreen dpy)
  win <- mkWin dpy rootw
  selectInput dpy win (exposureMask .|. buttonPressMask)
  mapWindow dpy win
  ilInit
  imgData <- readImage "/srv/photo/wrk/export/agneta_korkort.jpg"
  putStrLn $ show (bounds imgData)
  updateWin dpy win imgData
  return ()

updateWin dpy win imgData = do
  drawInWin dpy win imgData
  sync dpy True
  allocaXEvent $ \e -> do
    nextEvent dpy e
    ev <- getEvent e
    putStrLn $ eventName ev
    if ev_event_type ev /= buttonPress then updateWin dpy win imgData else return ()

mkWin dpy rootw = do
  col <- initColor dpy "#444444"
  createSimpleWindow dpy rootw 0 0 100 100 1 col col

drawInWin dpy win imgData = do
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
  img <- mkImg dpy vis iw ih depth imgData
  putImage dpy p gc img 0 0 (fromIntegral $ (w-iw) `div` 2) (fromIntegral $ (h-ih) `div` 2) iw ih
  copyArea dpy p win gc 0 0 w h 0 0
  freeGC dpy gc
  freePixmap dpy p

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros,real) <- allocNamedColor dpy colormap color
  return $ color_pixel apros

mkImg :: Display -> Visual -> Dimension -> Dimension -> CInt -> UArray (Int,Int,Int) Word8 -> IO Image
mkImg dpy vis w h depth imgData = do
  --let bs = (0, 3 * (fromIntegral h-1) * (fromIntegral w-1))
  ar <- newArray bs 50 :: IO (StorableArray Int CChar)
  ar <- newListArray bs ls :: IO (StorableArray Int CChar)


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
    createImage dpy vis depth zPixmap 0 ptr w h 32 0

    where
      bs = (0, (4 * (fromIntegral h) * (fromIntegral w)) - 1)
      f :: Int -> (Int,Int,Int)
      f n = (0,0,0)
      ar :: UArray Int Word8
      ar = ixmap bs f imgData
      ls :: [CChar]
      ls = map fromIntegral $ elems ar
