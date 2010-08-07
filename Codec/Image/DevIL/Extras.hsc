-- vim: syntax=haskell

module Codec.Image.DevIL.Extras (
  Image (..),
  loadImage,
  unloadImage
) where

import Control.Applicative
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.Storable
import Data.Array.Unboxed
import Data.Bits
import Data.Int (Int32)
import Data.Word (Word8, Word32, Word64)
import Debug.Trace
import Foreign.C
import Foreign.C.Types
import Foreign hiding (newArray)
import Control.Monad.Error

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

data Image = Image {
  imgName   :: ImageName,
  imgHeight :: Int,
  imgWidth  :: Int,
  imgBpp    :: Int,
  imgData   :: StorableArray (Int,Int,Int) Word8
}

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

unloadImage :: Image -> IO ()
unloadImage img = ilDeleteImages [imgName img]

loadImage :: String -> IO (Maybe Image)
loadImage filePath = do
  [name] <- ilGenImages 1
  ilBindImage name
  ilLoadImage filePath
  cols <- fmap fromIntegral $ ilGetIntegerC il_IMAGE_WIDTH
  rows <- fmap fromIntegral $ ilGetIntegerC il_IMAGE_HEIGHT
  bpp  <- fmap fromIntegral $ ilGetIntegerC il_IMAGE_BPP
  f    <- fmap fromIntegral $ ilGetIntegerC il_IMAGE_FORMAT
  if (cols <= 1 || rows <= 1)
    then do
      ilDeleteImages [name]
      return Nothing
    else do
      let bounds = ((0,0,0), (rows-1, cols-1, bpp-1))
      ptr <- ilGetDataC
      fptr <- newForeignPtr_ ptr
      dat <- unsafeForeignPtrToStorableArray fptr bounds
      return $ Just $ Image {
        imgName = name, imgHeight = fromIntegral rows,
        imgWidth = fromIntegral cols, imgBpp = fromIntegral bpp,
        imgData = dat
      }
