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

drawInWin :: Display -> Window -> UArray (Int,Int,Int) Word8 -> IO ()
drawInWin dpy win imgData = do
  bgcolor <- initColor dpy "#444444"
  gc <- createGC dpy win
  (_,_,_,w,h,_,_) <- getGeometry dpy win
  let depth = defaultDepthOfScreen (defaultScreenOfDisplay dpy)
      vis = defaultVisual dpy (defaultScreen dpy)
      ((_,_,_),(ih,iw,_)) = bounds imgData
      scale = 4
      dw = fromIntegral $ min (w `div` 2) $ fromIntegral (iw `div` scale)
      dh = fromIntegral $ min (h `div` 2) $ fromIntegral (ih `div` scale)
  p <- createPixmap dpy win w h depth
  setForeground dpy gc bgcolor
  fillRectangle dpy p gc 0 0 w h
  img <- mkImg dpy vis dw dh depth imgData (fromIntegral scale)
  putImage dpy p gc img 0 0 (fromIntegral $ (w-dw) `div` 2) (fromIntegral $ (h-dh) `div` 2) dw dh
  copyArea dpy p win gc 0 0 w h 0 0
  freeGC dpy gc
  freePixmap dpy p

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros,real) <- allocNamedColor dpy colormap color
  return $ color_pixel apros

mkImg :: Display -> Visual -> Dimension -> Dimension -> CInt -> UArray (Int,Int,Int) Word8 -> Int -> IO Image
mkImg dpy vis w h depth imgData scale = do
  ar <- newListArray bs ls :: IO (StorableArray Int CChar)
  withStorableArray ar $ \ptr ->
    createImage dpy vis depth zPixmap 0 ptr w h 32 0

    where
      ((_,_,_),(ih,iw,_)) = bounds imgData
      bs = (0, (4 * (fromIntegral h) * (fromIntegral w)) - 1)
      f :: Int -> (Int,Int,Int)
      f n = (ih-(scale*r), iw-(scale*c), l)
        where
          c = (n `div` 4) `mod` (fromIntegral w)
          r = (n `div` 4) `div` (fromIntegral w)
          l = case n `mod` 4 of
                0 -> 2
                2 -> 0
                m -> m
      ar = ixmap bs f imgData
      ls = map fromIntegral $ elems ar
