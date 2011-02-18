module PvdMonad (
  Pvd,
  PvdConf,
  State(..),
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
  setIdx
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

data State = State {
  stIdx :: TVar Int,
  stPlaylist :: TVar [String],
  stDpy :: Display,
  stWin :: Window,
  stImgCache :: TVar [(String, CachedImg)],
  stImgCacheSize :: Int,
  stSocket :: Socket
}

data PvdConf = PvdConf {
  cIdx :: TVar Int,
  cPlaylist :: TVar [String],
  cDpy :: Display,
  cWin :: Window,
  cImgCache :: TVar [(String, CachedImg)],
  cImgCacheSize :: Int,
  cSocket :: Socket
}

data CachedImg = CachedImg XImg | LoadingImg | LoadFailed

type Pvd = ReaderT PvdConf STM

runPvd :: PvdConf -> Pvd a -> IO a
runPvd conf pvd = atomically $ runReaderT pvd conf

initPvd playlist dpy win cacheSize socket = do
  cache <- newTVarIO []
  idx <- newTVarIO 0
  pl <- newTVarIO playlist
  let conf = PvdConf {
    cIdx = idx, cPlaylist = pl, cDpy = dpy, cWin = win,
    cImgCache = cache, cImgCacheSize = cacheSize, cSocket = socket
  }
  return conf

getSocket :: Pvd Socket
getSocket = liftM cSocket ask

getDpy :: Pvd Display
getDpy = liftM cDpy ask

getWin :: Pvd Window
getWin = liftM cWin ask

getPlaylist :: Pvd [String]
getPlaylist = liftM cPlaylist ask >>= (lift . readTVar)

setPlaylist :: [String] -> Pvd Bool
setPlaylist p = do
  pl <- liftM cPlaylist ask
  p0 <- lift $ readTVar pl
  lift $ writeTVar pl p
  return (p /= p0)

getIdx :: Pvd Int
getIdx = liftM cIdx ask >>= (lift . readTVar)

setIdx :: Int -> Pvd Bool
setIdx i = do
  idx <- liftM cIdx ask
  i0 <- lift $ readTVar idx
  lift $ writeTVar idx i
  return (i /= i0)

currentImage = do
  idx <- getIdx
  pl <- getPlaylist
  path <- if idx >= 0 && idx < length pl then return (pl !! idx) else lift retry
  cache <- liftM cImgCache ask >>= (lift . readTVar)
  case lookup path cache of
    Just (CachedImg img) -> return img
    _ -> lift retry

fetchNextPath = do
  pl <- getPlaylist
  cache <- liftM cImgCache ask >>= (lift . readTVar)
  idx <- getIdx
  sz <- liftM cImgCacheSize ask
  let paths = take sz (sortBy (comparePaths pl idx) pl) \\ fst (unzip cache)
  if null paths then lift retry else return (head paths)

putImgInCache img path = do
  c <- liftM cImgCache ask
  cache <- lift $ readTVar c
  pl <- getPlaylist
  idx <- getIdx
  cacheSize <- liftM cImgCacheSize ask
  let cache' = (path,img) : filter ((path /=) . fst) cache
      scache = sortBy (comparePaths pl idx `on` fst) cache'
  lift $ writeTVar c (take cacheSize scache)
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
