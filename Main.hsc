-- vim: syntax=haskell

module Main (
  main,
  scale
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


-----------


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
  w <- ilGetIntegerC il_IMAGE_WIDTH
  h <- ilGetIntegerC il_IMAGE_HEIGHT
  bpp <- ilGetIntegerC il_IMAGE_BPP
  f <- ilGetIntegerC il_IMAGE_FORMAT
  let bounds = ((0,0,0), (fromIntegral w-1, fromIntegral h-1, (fromIntegral bpp)-1))
  putStrLn $ "BPP: "++(show bpp)++", W: "++(show w)++", H: "++(show h)++", F: "++(show f)
  ptr <- ilGetDataC
  fptr <- newForeignPtr_ ptr
  dat <- unsafeForeignPtrToStorableArray fptr bounds
  return $ Img {
    imgName = name, imgHeight = fromIntegral h, 
    imgWidth = fromIntegral w, imgBpp = fromIntegral bpp,
    imgData = dat
  }

-----------

data State = State {
  dpy  :: Display,
  win  :: Window,
  img  :: Img,
--  img  :: UArray (Int,Int,Int) Word8,
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
--  img' <- readImage "/srv/photo/export/other/lovisa_grattis.tif"
  let sc' = (1,5)
--  ximg' <- mkImg dpy' img' sc'
--  img' <- loadImage "/srv/photo/export/other/lovisa_grattis.tif"
  img' <- loadImage "/srv/photo/export/other/Lovisa/Lovisa-1.jpg"
  ximg' <- makeXImage dpy' img' (imgWidth img') (imgHeight img') id
  --ximg' <- makeXImage dpy' img' 100 200 id
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
--  UArray (Int,Int,Int) Word8 ->
  Img ->
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
      (viewWidth,viewHeight) = scale s (imgWidth imgData,imgHeight imgData)
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


makeXImage :: Display -> Img -> Int -> Int -> ((Int,Int) -> (Int,Int)) -> IO Image
makeXImage dpy img w' h' fxy = do
  xImgData <- mapIndices bs mapIdx (imgData img)
  withStorableArray xImgData (ci . castPtr)
  where
    ci p = createImage dpy vis depth zPixmap 0 p (fi w') (fi h') 32 0
    depth = defaultDepthOfScreen (defaultScreenOfDisplay dpy)
    vis = defaultVisual dpy (defaultScreen dpy)
    bs  = ((0,0,0), (w'-1,h'-1,3))
    fi = fromIntegral
    mapIdx (x,y,c) = (x', y', c' c)
      where
        (x',y') = fxy (imgWidth img - x - 1, imgHeight img - y - 1)
        c' 0 = 2
        c' 1 = 1
        c' 2 = 0
        c' 3 = 0


mkImg' ::
  Display ->
  Img ->
  Scale ->
  IO Image
mkImg' dpy img sc = do
  putStrLn $ "Depth: "++(show depth)
  withStorableArray (imgData img) $ \ptr ->
    createImage dpy vis depth zPixmap 0 (castPtr ptr) (fromIntegral w) (fromIntegral h) 32 0

    where
      depth = defaultDepthOfScreen (defaultScreenOfDisplay dpy)
      vis = defaultVisual dpy (defaultScreen dpy)
      w = imgWidth img
      h = imgHeight img

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

