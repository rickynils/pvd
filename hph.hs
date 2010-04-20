newtype Image = Image {
  imgName   :: Word32,
  imgHeight :: Int,
  imgWidth  :: Int,
  imgBpp    :: Int,
  imgData   :: StorableArray Int Word8
}

loadImage :: String -> IO Image
loadImage filePath = do
  name <- ilGenImageNameC
  ilBindImage name
  ilLoadImage name
  height <-
  width <-
  bpp <-
  ptr <- ilGetData
  let size = imgHeight*imgWidth*imgBpp
  dat <- unsafeForeignPtrToStorableArray ptr (0,size-1)
  return $ Image {
    imgName = name, imgHeight = height, imgWidth = width, 
    imgBpp = bpp, imgData = dat
  }
