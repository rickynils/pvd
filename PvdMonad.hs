module PvdMonad (
  Pvd,
  PvdConf,
  CachedImg(..),
  initPvd,
  runPvd,
  getSocket,
  currentImage,
  fetchNextPath,
  putImgInCache,
  modPlaylist,
  getPlaylist,
  setPlaylist,
  getWin,
  getDpy,
  modIdx,
  getIdx,
  setIdx,
  notifyChange,
  waitForChange
) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad (liftM)
import Data.Ord (comparing)
import Data.Function (on)
import Data.List ((\\), sortBy, elemIndex)
import Graphics.X11.Xlib (Display, Window)
import Network.Socket (Socket)
import XUtils (XImg)

data PvdConf = PvdConf {
  cIdx :: TVar Int,
  cPlaylist :: TVar [String],
  cDpy :: Display,
  cWin :: Window,
  cImgCache :: TVar [(String, CachedImg)],
  cImgCacheSize :: Int,
  cSocket :: Socket,
  cChanges :: TVar Int
}

data CachedImg = CachedImg XImg | LoadingImg | LoadFailed

type Pvd = ReaderT PvdConf STM

runPvd :: PvdConf -> Pvd a -> IO a
runPvd conf pvd = atomically $ runReaderT pvd conf

initPvd playlist dpy win cacheSize socket = do
  cache <- newTVarIO []
  idx <- newTVarIO 0
  c <- newTVarIO 1
  pl <- newTVarIO playlist
  let conf = PvdConf {
    cIdx = idx, cPlaylist = pl, cDpy = dpy, cWin = win, cImgCache = cache,
    cImgCacheSize = cacheSize, cSocket = socket, cChanges = c
  }
  return conf

readT :: (PvdConf -> TVar a) -> Pvd a
readT f = liftM f ask >>= (lift . readTVar)

writeT :: (PvdConf -> TVar a) -> a -> Pvd ()
writeT f x = liftM f ask >>= (lift . flip writeTVar x)

modT f g = do
  x <- readT f
  writeT f (g x)

getSocket :: Pvd Socket
getSocket = liftM cSocket ask

getDpy :: Pvd Display
getDpy = liftM cDpy ask

getWin :: Pvd Window
getWin = liftM cWin ask

getPlaylist :: Pvd [String]
getPlaylist = readT cPlaylist

setPlaylist :: [String] -> Pvd Bool
setPlaylist p = do
  p0 <- readT cPlaylist
  writeT cPlaylist p
  return (p /= p0)

getIdx :: Pvd Int
getIdx = readT cIdx

setIdx :: Int -> Pvd Bool
setIdx i = do
  i0 <- readT cIdx
  writeT cIdx i
  return (i /= i0)

currentImage = do
  idx <- getIdx
  pl <- getPlaylist
  path <- if idx >= 0 && idx < length pl then return (pl !! idx) else lift retry
  cache <- readT cImgCache
  case lookup path cache of
    Just (CachedImg img) -> return img
    _ -> lift retry

fetchNextPath = do
  pl <- getPlaylist
  cache <- readT cImgCache
  idx <- getIdx
  sz <- liftM cImgCacheSize ask
  let paths = take sz (sortBy (comparePaths pl idx) pl) \\ fst (unzip cache)
  if null paths then lift retry else return (head paths)

putImgInCache img path = do
  cache <- readT cImgCache
  pl <- getPlaylist
  idx <- getIdx
  cacheSize <- liftM cImgCacheSize ask
  let cache' = (path,img) : filter ((path /=) . fst) cache
      scache = sortBy (comparePaths pl idx `on` fst) cache'
  writeT cImgCache (take cacheSize scache)
  return path

comparePaths playlist idx p1 p2 = case (i1,i2) of
  (Nothing, Nothing) -> EQ
  (Nothing, Just _) -> GT
  (Just _, Nothing) -> LT
  (Just n1, Just n2) | idx `elem` [n1,n2] -> comparing abs (n1-idx) (n2-idx)
  (Just n1, Just n2) -> comparing abs (n1-idx-1) (n2-idx-1)
  where
    i1 = elemIndex p1 playlist
    i2 = elemIndex p2 playlist

modIdx f = do
  i <- getIdx
  l <- liftM length getPlaylist
  setIdx $ max 0 (min (l-1) (f (l,i)))

modPlaylist f = liftM f getPlaylist >>= setPlaylist

notifyChange = modT cChanges (+ 1)

waitForChange = do
  c <- readT cChanges
  when (c <= 0) (lift retry)
  writeT cChanges 0
