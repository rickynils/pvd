-- vim: syntax=haskell

module Main (
  main
) where

import Data.Word (Word8, Word32, Word64)
import Data.Int (Int32)
import Data.Maybe
import Data.Foldable (for_)
import Foreign hiding (newArray)
import Foreign.C
import Codec.Image.DevIL (ilInit, readImage)
import Control.Concurrent
import Control.Monad
import Control.Exception
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
import System.IO
import Network.Socket

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

data XImg = XImg {
  xImg :: X.Image,
  xImgH :: Int,
  xImgW :: Int
}

loadImage :: String -> IO (Maybe Img)
loadImage filePath = do
  [name] <- ilGenImages 1
  ilBindImage name
  ilLoadImage filePath
  cols <- ilGetIntegerC il_IMAGE_WIDTH
  rows <- ilGetIntegerC il_IMAGE_HEIGHT
  bpp  <- ilGetIntegerC il_IMAGE_BPP
  f    <- ilGetIntegerC il_IMAGE_FORMAT
  if cols <= 1 || rows <= 1
    then ilDeleteImages [name] >> return Nothing
    else do
      let bounds = ((0,0,0), (fromIntegral rows-1, fromIntegral cols-1, (fromIntegral bpp)-1))
      ptr <- ilGetDataC
      fptr <- newForeignPtr_ ptr
      dat <- unsafeForeignPtrToStorableArray fptr bounds
      return $ Just $ Img {
        imgName = name, imgHeight = fromIntegral rows,
        imgWidth = fromIntegral cols, imgBpp = fromIntegral bpp,
        imgData = dat
      }

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

initColor :: X.Display -> String -> IO X.Pixel
initColor dpy color = do
  let colormap = X.defaultColormap dpy (X.defaultScreen dpy)
  (apros,real) <- X.allocNamedColor dpy colormap color
  return $ X.color_pixel apros

makeXImage :: X.Display -> Img -> IO XImg
makeXImage dpy img = do
  xImgData <- mapIndices bs mapIdx (imgData img)
  withStorableArray xImgData (ci . castPtr)
  where
    w = fromIntegral (imgWidth img)
    h = fromIntegral (imgHeight img)
    ci p = do
      ximg <- X.createImage dpy vis depth X.zPixmap 0 p w h 32 0
      return XImg { xImg = ximg, xImgH = imgHeight img, xImgW = imgWidth img }
    depth = X.defaultDepthOfScreen (X.defaultScreenOfDisplay dpy)
    vis = X.defaultVisual dpy (X.defaultScreen dpy)
    bs = ((0,0,0), (imgHeight img - 1, imgWidth img - 1, 3))
    mapIdx (y,x,c) = (imgHeight img - y - 1, imgWidth img - x - 1, c' c)
      where
        c' 0 = 2
        c' 1 = 1
        c' 2 = 0
        c' 3 = 0

loadXImg :: X.Display -> String -> IO (Maybe XImg)
loadXImg dpy path = do
  img <- loadImage path
  if isNothing img
    then return Nothing
    else do
      ximg <- makeXImage dpy (fromJust img)
      ilDeleteImages [imgName (fromJust img)]
      return $ Just ximg

main = do
  imgPath <- newEmptyMVar
  (dpy,win) <- initX
  forkIO (eventLoop imgPath dpy win)
  initSocket "4245" >>= socketLoop (\p -> putMVar imgPath p >> expose dpy win)

expose dpy win = X.allocaXEvent $ \e -> do
  X.setEventType e X.expose
  X.sendEvent dpy win False X.noEventMask e
  X.flush dpy

initX = do
  dpy <- X.openDisplay ""
  rootw <- X.rootWindow dpy (X.defaultScreen dpy)
  win <- mkWin dpy rootw
  X.selectInput dpy win X.exposureMask
  X.mapWindow dpy win
  ilInit
  return (dpy,win)
  where
    mkWin dpy rootw = do
      col <- initColor dpy "#444444"
      X.createSimpleWindow dpy rootw 0 0 100 100 1 col col

initSocket port = withSocketsDo $ do
  addrinfos <- getAddrInfo
    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
    Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 5
  return sock

socketLoop onNewImg sock = do
  (connsock, clientaddr) <- accept sock
  forkIO $ procMessages connsock clientaddr
  socketLoop onNewImg sock
    where
      procMessages connsock clientaddr = do
        connhdl <- socketToHandle connsock ReadMode
        hSetBuffering connhdl LineBuffering
        messages <- hGetContents connhdl
        mapM_ onNewImg (lines messages)
        hClose connhdl

eventLoop imgPath dpy win = innerLoop Nothing Nothing
  where
    innerLoop path ximg = do
      path' <- tryTakeMVar imgPath
      ximg' <- if path == path' || isNothing path'
                 then return ximg
                 else loadXImg dpy (fromJust path')
      drawImg dpy win ximg'
      X.sync dpy True
      X.allocaXEvent $ \e -> do
        X.nextEvent dpy e
        innerLoop path' ximg'
