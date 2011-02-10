module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (when, forever)
import Data.Function (on)
import Data.List ((\\), sortBy, elemIndex)
import Data.Maybe (maybe)
import Data.Ord (comparing)
import qualified Codec.Image.DevIL as IL (ilInit)
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as X
import qualified XUtils as X
import System.Console.GetOpt
import System.Exit (exitSuccess)
import System (getArgs)
import PvdSocket (initSocket, handleClient)
import PvdState

data Flag =
  Port String | CacheSize Int | Playlist String | Help
    deriving (Eq)

main = do
  state <- initApp
  forkIO $ forever $ updateCache state
  forkIO $ forever $ handleEvent state
  forever $ handleClient state

initApp = do
  args <- getArgs
  (flags, fs1) <- parseOptions args
  fs2 <- fmap (concatMap words) $ sequence [readFile p | (Playlist p) <- flags]
  let cacheSize = last [c | CacheSize c <- CacheSize 15 : flags]
      port = last [p | Port p <- Port "4245" : flags]
  IL.ilInit
  (dpy,win) <- X.initX
  socket <- initSocket port
  cache <- newTVarIO []
  idx <- newTVarIO 0
  playlist <- newTVarIO (fs1++fs2)
  return State {
    stIdx = idx, stPlaylist = playlist, stDpy = dpy, stWin = win,
    stImgCache = cache, stImgCacheSize = cacheSize, stSocket = socket
  }

parseOptions argv = case getOpt Permute options argv of
  (o,n,[]  ) -> if Help `elem` o then printHelp >> exitSuccess else return (o, n)
  (_,_,errs) -> printHelp >> fail (concatMap (filter ('\n' /=)) errs)

printHelp = mapM_ putStrLn
  [ "Usage:\n  pvd [OPTION...] [FILE...]\n"
  , "Photo Viewer Daemon - a daemon for viewing photos.\n"
  , usageInfo "Available options:" options
  ]

options =
  [ Option "h" ["help"] (NoArg Help) "print this help text"
  , Option "p" ["port"] (ReqArg Port "PORT") "photo viewer daemon port"
  , Option "c" ["cache"] (ReqArg (CacheSize . read) "SIZE") "photo cache size"
  , Option "l" ["playlist"] (ReqArg Playlist "PLAYLIST") "playlist file"
  ]

handleEvent state = do
  img <- atomically $ do
    idx <- readTVar (stIdx state)
    pl <- readTVar (stPlaylist state)
    path <- if idx >= 0 && idx < length pl then return (pl !! idx) else retry
    cache <- readTVar (stImgCache state)
    case lookup path cache of
      Just (CachedImg img) -> return img
      _ -> retry
  X.drawImg (stDpy state) (stWin state) img
  X.allocaXEvent $ \e -> X.nextEvent (stDpy state) e

updateCache state = do
  path <- atomically (fetchNextPath state >>= putImgInCache state LoadingImg)
  img <- fmap (maybe LoadFailed CachedImg) (X.loadXImg (stDpy state) path)
  atomically (putImgInCache state img path)

fetchNextPath state = do
  pl <- readTVar (stPlaylist state)
  cache <- readTVar (stImgCache state)
  idx <- readTVar (stIdx state)
  let paths = take sz (sortBy (comparePaths pl idx) pl) \\ fst (unzip cache)
      sz = stImgCacheSize state
  if null paths then retry else return (head paths)

putImgInCache state img path = do
  cache <- readTVar (stImgCache state)
  pl <- readTVar (stPlaylist state)
  idx <- readTVar (stIdx state)
  let cache' = (path,img) : filter ((path /=) . fst) cache
      scache = sortBy (comparePaths pl idx `on` fst) cache'
  writeTVar (stImgCache state) (take (stImgCacheSize state) scache)
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
