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
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as X
import System.Exit (exitWith, ExitCode(..))

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

foreign import CALLTYPE "ilDeleteImages" ilDeleteImagesC
    :: ILsizei -> Ptr ILuint -> IO ()

ilDeleteImages :: [ImageName] -> IO ()
ilDeleteImages names = do
    ar <- newListArray (0, length names-1) (fromImageName <$> names)
    withStorableArray ar $ \p -> do
        ilDeleteImagesC (fromIntegral $ length names) p

data Img = Img {
  imgName   :: ImageName,
  imgHeight :: Int,
  imgWidth  :: Int,
  imgBpp    :: Int,
  imgData   :: StorableArray (Int,Int,Int) Word8
}

data State = State {
  stDpy       :: X.Display,
  stWin       :: X.Window,
  stImg       :: Img,
  stXImg      :: Maybe X.Image,
  stXImgWidth  :: Int,
  stXImgHeight :: Int
}

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

initState :: IO State
initState = do
  dpy <- X.openDisplay ""
  rootw <- X.rootWindow dpy (X.defaultScreen dpy)
  win <- mkWin dpy rootw
  X.selectInput dpy win (X.exposureMask .|. X.buttonPressMask .|. X.keyPressMask)
  X.mapWindow dpy win
  ilInit
  img <- loadImage "/srv/photo/export/other/Lovisa/Lovisa-1.jpg"
  remakeXImage State { stDpy = dpy, stWin = win, stImg = img, stXImg = Nothing, stXImgWidth = 0, stXImgHeight = 0}

main :: IO ()
main = initState >>= updateWin >>= cleanUp

cleanUp s = ilDeleteImages [imgName (stImg s)]

half (x,y) = (2*x, 2*y)

updateWin s = do
  s' <- remakeXImage s
  let (Just ximg) = stXImg s'
  drawImg (stDpy s') (stWin s') ximg (stXImgWidth s') (stXImgHeight s')
  X.sync (stDpy s') True
  X.allocaXEvent $ \e -> do
    X.nextEvent (stDpy s') e
    ev <- X.getEvent e
    handleEvent s' $ X.ev_event_type ev
  where
    handleEvent s ev | ev == X.buttonPress = return s
                     | ev == X.keyPress = updateWin s
                     | otherwise = updateWin s

remakeXImage s@(State { stDpy = dpy, stWin = win, stImg = img, stXImg = ximg, stXImgWidth = ximgw, stXImgHeight = ximgh }) = do
  (_,_,_,winWidth,winHeight,_,_) <- X.getGeometry dpy win
  let w = min (fromIntegral winWidth) (imgWidth img)
      h = min (fromIntegral winHeight) (imgHeight img)
  if ximg == Nothing || w /= ximgw || h /= ximgh
    then do
      ximg <- makeXImage dpy img w h id
      return s { stXImg = Just ximg, stXImgWidth = w, stXImgHeight = h }
    else return s

mkWin dpy rootw = do
  col <- initColor dpy "#444444"
  X.createSimpleWindow dpy rootw 0 0 100 100 1 col col

drawImg :: X.Display -> X.Window -> X.Image -> Int -> Int -> IO ()
drawImg dpy win ximg w h = do
  bgcolor <- initColor dpy "#444444"
  gc <- X.createGC dpy win
  (_,_,_,winWidth,winHeight,_,_) <- X.getGeometry dpy win
  let depth = X.defaultDepthOfScreen (X.defaultScreenOfDisplay dpy)
      vis = X.defaultVisual dpy (X.defaultScreen dpy)
      portWidth = fromIntegral w
      portHeight = fromIntegral h
      portX = fromIntegral ((winWidth-portWidth) `div` 2)
      portY = fromIntegral ((winHeight-portHeight) `div` 2)
  pixmap <- X.createPixmap dpy win winWidth winHeight depth
  X.setForeground dpy gc bgcolor
  X.fillRectangle dpy pixmap gc 0 0 winWidth winHeight
  X.putImage dpy pixmap gc ximg 0 0 portX portY portWidth portHeight
  X.copyArea dpy pixmap win gc 0 0 winWidth winHeight 0 0
  X.freeGC dpy gc
  X.freePixmap dpy pixmap

initColor :: X.Display -> String -> IO X.Pixel
initColor dpy color = do
  let colormap = X.defaultColormap dpy (X.defaultScreen dpy)
  (apros,real) <- X.allocNamedColor dpy colormap color
  return $ X.color_pixel apros

makeXImage :: X.Display -> Img -> Int -> Int -> ((Int,Int) -> (Int,Int)) -> IO X.Image
makeXImage dpy img w h fxy = do
  xImgData <- mapIndices bs mapIdx (imgData img)
  withStorableArray xImgData (ci . castPtr)
  where
    ci p = X.createImage dpy vis depth X.zPixmap 0 p (fi w) (fi h) 32 0
    depth = X.defaultDepthOfScreen (X.defaultScreenOfDisplay dpy)
    vis = X.defaultVisual dpy (X.defaultScreen dpy)
    bs = ((0,0,0), (h-1,w-1,3))
    fi = fromIntegral
    mapIdx (y,x,c) = (imgHeight img - y' - 1, imgWidth img - x' - 1, c' c)
      where
        (x',y') = fxy (x,y)
        c' 0 = 2
        c' 1 = 1
        c' 2 = 0
        c' 3 = 0
