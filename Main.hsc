-- vim: syntax=haskell

module Main (
  main
) where

import Data.Word (Word8, Word32, Word64)
import Data.Int (Int32)
import Foreign hiding (newArray)
import Foreign.C
import Codec.Image.DevIL (ilInit, readImage)
import Control.Concurrent (threadDelay)
import Control.Applicative
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

data Img = Img {
  imgName   :: ImageName,
  imgHeight :: Int,
  imgWidth  :: Int,
  imgBpp    :: Int,
  imgData   :: StorableArray (Int,Int,Int) Word8
}

#include "IL/il.h"

type ILuint     = #type ILuint
type ILsizei    = #type ILsizei
type ILboolean  = #type ILboolean
type ILenum     = #type ILenum
type ILint      = #type ILint
type ILubyte    = #type ILubyte

il_BGR = (#const IL_BGR) :: ILenum
il_BGRA = (#const IL_BGRA) :: ILenum
il_RGB = (#const IL_RGB) :: ILenum
il_RGBA = (#const IL_RGBA) :: ILenum
il_UNSIGNED_BYTE = (#const IL_UNSIGNED_BYTE) :: ILenum
il_IMAGE_HEIGHT = (#const IL_IMAGE_HEIGHT) :: ILenum
il_IMAGE_WIDTH  = (#const IL_IMAGE_WIDTH)  :: ILenum
il_IMAGE_BPP  = (#const IL_IMAGE_BPP)  :: ILenum
il_IMAGE_FORMAT  = (#const IL_IMAGE_FORMAT)  :: ILenum

newtype ImageName = ImageName { fromImageName :: ILuint }

foreign import CALLTYPE "ilBindImage" ilBindImageC :: ILuint -> IO ()

ilBindImage :: ImageName -> IO ()
ilBindImage (ImageName name) = ilBindImageC name

foreign import CALLTYPE "ilLoadImage" ilLoadImageC :: CString -> IO ILboolean

ilLoadImage :: FilePath -> IO Bool
ilLoadImage file = do
    (0 /=) <$> withCString file ilLoadImageC

foreign import CALLTYPE "ilGenImages" ilGenImagesC
  :: ILsizei -> Ptr ILuint -> IO ()

ilGenImages :: Int -> IO [ImageName]
ilGenImages num = do
    ar <- newArray (0, num-1) 0
    withStorableArray ar $ \p -> do
        ilGenImagesC (fromIntegral num) p
    map ImageName <$> getElems ar

foreign import CALLTYPE "ilGetInteger" ilGetIntegerC
    :: ILenum -> IO ILint

foreign import CALLTYPE "ilGetData" ilGetDataC
    :: IO (Ptr Word8)

loadImage :: String -> IO Img
loadImage filePath = do
  [name] <- ilGenImages 1
  ilBindImage name
  ilLoadImage filePath
  cols <- ilGetIntegerC il_IMAGE_WIDTH
  rows <- ilGetIntegerC il_IMAGE_HEIGHT
  bpp  <- ilGetIntegerC il_IMAGE_BPP
  f    <- ilGetIntegerC il_IMAGE_FORMAT
  let bounds = ((0,0,0), (fromIntegral rows-1, fromIntegral cols-1, (fromIntegral bpp)-1))
  putStrLn $ "BPP: "++(show bpp)++", W: "++(show cols)++", H: "++(show rows)++", F: "++(show f)
  ptr <- ilGetDataC
  fptr <- newForeignPtr_ ptr
  dat <- unsafeForeignPtrToStorableArray fptr bounds
  return $ Img {
    imgName = name, imgHeight = fromIntegral rows, 
    imgWidth = fromIntegral cols, imgBpp = fromIntegral bpp,
    imgData = dat
  }

data State = State {
  dpy  :: Display,
  win  :: Window,
  img  :: Img,
  ximg :: Image,
  xoff :: Int,
  yoff :: Int,
  imgw :: Int,
  imgh :: Int
}

initState :: IO State
initState = do
  dpy' <- openDisplay ""
  rootw <- rootWindow dpy' (defaultScreen dpy')
  win' <- mkWin dpy' rootw
  selectInput dpy' win' (exposureMask .|. buttonPressMask .|. keyPressMask)
  mapWindow dpy' win'
  ilInit
  img' <- loadImage "/srv/photo/export/other/Lovisa/Lovisa-1.jpg"
  let w = imgWidth img' `div` 2
  let h = imgHeight img' `div` 2
  ximg' <- makeXImage dpy' img' w h half
  return State { dpy = dpy', win = win', img = img', ximg = ximg', xoff = 0, yoff = 0, imgw = w, imgh = h}

main :: IO ()
main = initState >>= updateWin

half (x,y) = (2*x, 2*y)

updateWin s = do
  drawInWin (dpy s) (win s) (ximg s) (xoff s) (yoff s) (imgw s) (imgh s)
  sync (dpy s) True
  allocaXEvent $ \e -> do
    nextEvent (dpy s) e
    ev <- getEvent e
    handleEvent $ ev_event_type ev
  where
    handleEvent ev | ev == buttonPress = return ()
                   | ev == keyPress = updateWin s { xoff = xoff s + 1 }
                   | otherwise = updateWin s

mkWin dpy rootw = do
  col <- initColor dpy "#444444"
  createSimpleWindow dpy rootw 0 0 100 100 1 col col

drawInWin ::
  Display ->
  Window ->
  Image ->
  Int ->
  Int ->
  Int ->
  Int ->
  IO ()
drawInWin dpy win img x y w h = do
  bgcolor <- initColor dpy "#444444"
  gc <- createGC dpy win
  (_,_,_,winWidth,winHeight,_,_) <- getGeometry dpy win
  let depth = defaultDepthOfScreen (defaultScreenOfDisplay dpy)
      vis = defaultVisual dpy (defaultScreen dpy)
      portWidth = min (winWidth) (fromIntegral w)
      portHeight = min (winHeight) (fromIntegral h)
      portX = 0
      portY = 0
      viewX = 0
      viewY = 0
  pixmap <- createPixmap dpy win winWidth winHeight depth
  setForeground dpy gc bgcolor
  fillRectangle dpy pixmap gc 0 0 winWidth winHeight
  putImage dpy pixmap gc img viewX viewY portX portY portWidth portHeight
  copyArea dpy pixmap win gc 0 0 winWidth winHeight 0 0
  freeGC dpy gc
  freePixmap dpy pixmap

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros,real) <- allocNamedColor dpy colormap color
  return $ color_pixel apros

makeXImage :: Display -> Img -> Int -> Int -> ((Int,Int) -> (Int,Int)) -> IO Image
makeXImage dpy img w h fxy = do
  xImgData <- mapIndices bs mapIdx (imgData img)
  withStorableArray xImgData (ci . castPtr)
  where
    ci p = createImage dpy vis depth zPixmap 0 p (fi w) (fi h) 32 0
    depth = defaultDepthOfScreen (defaultScreenOfDisplay dpy)
    vis = defaultVisual dpy (defaultScreen dpy)
    bs = ((0,0,0), (h-1,w-1,3))
    fi = fromIntegral
    mapIdx (y,x,c) = (imgHeight img - y' - 1, imgWidth img - x' - 1, c' c)
      where
        (x',y') = fxy (x,y)
        c' 0 = 2
        c' 1 = 1
        c' 2 = 0
        c' 3 = 0
