-- vim: syntax=haskell

module Codec.Image.DevIL.Extras (
  il_BGR,
  il_BGRA,
  ilBindImage,
  ilDeleteImages,
  ilGenImages,
  ilGetDataC,
  ilGetIntegerC,
  il_IMAGE_BPP,
  il_IMAGE_FORMAT,
  il_IMAGE_HEIGHT,
  il_IMAGE_WIDTH,
  ilLoadImage,
  il_RGB,
  il_RGBA,
  ImageName
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
