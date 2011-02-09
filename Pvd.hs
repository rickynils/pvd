-- vim: syntax=haskell

module Main (
  main
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Trans
import Data.Foldable (notElem)
import Data.Maybe
import Data.List
import Data.Function (on)
import Network.Socket
import Prelude hiding (notElem)
import qualified Codec.Image.DevIL as IL (ilInit)
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as X
import System.Exit (exitSuccess)
import System.IO
import System (getArgs)
import System.Console.GetOpt
import XUtils

type Index = TVar Int
type Playlist = TVar [String]
type Cache = TVar [(String, CachedImg)]

data CachedImg = CachedImg XImg | LoadingImg | LoadFailed

data State = State {
  stIdx :: Index,
  stPlaylist :: Playlist,
  stDpy :: X.Display,
  stWin :: X.Window,
  stImgCache :: Cache,
  stImgCacheSize :: Int,
  stSocket :: Socket
}

data Flag =
  Port String | CacheSize Int | Playlist String | Help
    deriving (Eq)

main = do
  args <- getArgs
  (flags, files1) <- parseOptions args
  files2 <- readPlaylist flags
  let cacheSize = last [c | CacheSize c <- CacheSize 15 : flags]
      port = last [p | Port p <- Port "4245" : flags]
  IL.ilInit
  (dpy,win) <- initX
  socket <- initSocket port
  cache <- newTVarIO []
  idx <- newTVarIO 0
  playlist <- newTVarIO (files1++files2)
  let state = State {
    stIdx = idx, stPlaylist = playlist, stDpy = dpy, stWin = win,
    stImgCache = cache, stImgCacheSize = cacheSize, stSocket = socket
  }
  forkIO $ cacheLoop state
  forkIO $ eventLoop state
  socketLoop state

readPlaylist fs = fmap (concatMap words) $ sequence [readFile pl | (Playlist pl) <- fs]

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

parseOptions argv = case getOpt Permute options argv of
  (o,n,[]  ) -> if Help `elem` o then printHelp >> exitSuccess else return (o, n)
  (_,_,errs) -> printHelp >> fail (concatMap (filter ('\n' /=)) errs)

initSocket port = withSocketsDo $ do
  addrinfos <- getAddrInfo
    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
    Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 5
  return sock

socketLoop state = do
  (connsock, clientaddr) <- accept (stSocket state)
  forkIO $ processMessages connsock clientaddr
  socketLoop state
    where
      processMessages connsock clientaddr = do
        connhdl <- socketToHandle connsock ReadMode
        hSetBuffering connhdl LineBuffering
        messages <- hGetContents connhdl
        redraw <- fmap or $ mapM (atomically . parseCmd state) (lines messages)
        when redraw $ sendExposeEvent (stDpy state) (stWin state)
        hClose connhdl

eventLoop state = do
  img <- atomically (fetchImage state)
  drawImg (stDpy state) (stWin state) img
  X.allocaXEvent $ \e -> X.nextEvent (stDpy state) e
  eventLoop state

cacheLoop state = do
  path <- atomically (fetchNextPath state >>= putImgInCache state LoadingImg)
  img <- fmap (maybe LoadFailed CachedImg) (loadXImg (stDpy state) path)
  atomically (putImgInCache state img path)
  cacheLoop state

fetchImage state = do
  idx <- readTVar (stIdx state)
  pl <- readTVar (stPlaylist state)
  path <- if idx >= 0 && idx < length pl then return (pl !! idx) else retry
  cache <- readTVar (stImgCache state)
  case lookup path cache of
    Just (CachedImg img) -> return img
    _ -> retry

fetchNextPath state = do
  pl <- readTVar (stPlaylist state)
  cache <- readTVar (stImgCache state)
  idx <- readTVar (stIdx state)
  let paths = (take sz (sortBy (comparePaths pl idx) pl)) \\ (fst $ unzip cache)
      sz = stImgCacheSize state
  if null paths then retry else return (head paths)

putImgInCache state img path = do
  cache <- readTVar (stImgCache state)
  pl <- readTVar (stPlaylist state)
  idx <- readTVar (stIdx state)
  let cache' = (path,img) : (filter ((path /=) . fst) cache)
      scache = sortBy (comparePaths pl idx `on` fst) cache'
  writeTVar (stImgCache state) (take (stImgCacheSize state) scache)
  return path

comparePaths playlist idx p1 p2 = case (i1,i2) of
  (Nothing, Nothing) -> EQ
  (Nothing, Just _) -> GT
  (Just _, Nothing) -> LT
  (Just n1, Just n2) | elem idx [n1,n2] -> (compare `on` abs) (n1-idx) (n2-idx)
  (Just n1, Just n2) -> (compare `on` abs) (n1-idx-1) (n2-idx-1)
  where
    i1 = elemIndex p1 playlist
    i2 = elemIndex p2 playlist

parseCmd :: State -> String -> STM Bool
parseCmd s@(State {stIdx = idx, stPlaylist = pl}) cmd = case words cmd of
  ["next"] -> gotoIdx s (+ 1)
  ["prev"] -> gotoIdx s (+ (-1))
  ["first"] -> gotoIdx s (\_ -> 0)
  ["last"] -> readTVar pl >>= (\p -> gotoIdx s (\_ -> length p - 1))
  "playlist":"add":imgs -> do
    fmap (++ imgs) (readTVar pl) >>= (writeTVar pl)
    return True
  "playlist":"replace":imgs -> do
    writeTVar idx 0
    writeTVar pl imgs
    return True
  "playlist":"insert":"0":imgs -> do
    p <- readTVar pl
    fmap (+ (length p)) (readTVar idx) >>= (writeTVar idx)
    writeTVar pl (imgs++p)
    return True
  _ -> return False

gotoIdx (State {stIdx = idx, stPlaylist = pl}) f = do
  i <- readTVar idx
  let i' = f i
  l <- fmap length (readTVar pl)
  when (i' /= i && i' >= 0 && i' < l) (writeTVar idx i')
  return (i' /= i)
