-- vim: syntax=haskell

module Main (
  main
) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Trans
import Data.Foldable (notElem)
import Data.Maybe
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

data Flag =
  Port String | CacheSize Int | Playlist String | Help
    deriving (Eq)

data State = State {
  stIdx :: Int,
  stPlaylist :: [String],
  stDpy :: X.Display,
  stWin :: X.Window,
  stImgCache :: [(String, XImg)],
  stLoadLock :: MVar (),
  stImgCacheSize :: Int
}

imgPath (State {stIdx = idx, stPlaylist = pl})
  | idx >= 0 && idx < length pl = Just (pl !! idx)
  | otherwise = Nothing

main = do
  args <- getArgs
  (flags, files1) <- parseOptions args
  files2 <- readPlaylist flags
  let cacheSize = last [c | CacheSize c <- CacheSize 15 : flags]
      port = last [p | Port p <- Port "4245" : flags]
  IL.ilInit
  (dpy,win) <- initX
  l <- newMVar ()
  state <- newMVar $ State {
    stIdx = 0, stPlaylist = files1++files2, stDpy = dpy, stWin = win,
    stImgCache = [], stLoadLock = l, stImgCacheSize = cacheSize
  }
  updateCache state
  forkIO $ eventLoop state
  initSocket port >>= socketLoop state

readPlaylist fs = fmap (concat . map words) $ sequence [readFile pl | (Playlist pl) <- fs]

printHelp = sequence_ $ map putStrLn $
  [ "Usage:\n  pvd [OPTION...] [FILE...]\n"
  , "Photo Viewer Daemon - a daemon for viewing photos.\n"
  , (usageInfo "Available options:" options)
  ]

options =
  [ Option ['h'] ["help"] (NoArg Help) "print this help text"
  , Option ['p'] ["port"] (ReqArg Port "PORT") "photo viewer daemon port"
  , Option ['c'] ["cache"] (ReqArg (CacheSize . read) "SIZE") "photo cache size"
  , Option ['l'] ["playlist"] (ReqArg Playlist "PLAYLIST") "playlist file"
  ]

parseOptions argv = case getOpt Permute options argv of
  (o,n,[]  ) -> if elem Help o then printHelp >> exitSuccess else return (o, n)
  (_,_,errs) -> printHelp >> fail (concat $ map (filter ('\n' /=)) errs)

initSocket port = withSocketsDo $ do
  addrinfos <- getAddrInfo
    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
    Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 5
  return sock

socketLoop state sock = do
  (connsock, clientaddr) <- accept sock
  forkIO $ processMessages connsock clientaddr
  socketLoop state sock
    where
      processMessages connsock clientaddr = do
        connhdl <- socketToHandle connsock ReadMode
        hSetBuffering connhdl LineBuffering
        messages <- hGetContents connhdl
        mapM_ runParseCmd (lines messages)
        hClose connhdl
      runParseCmd c = do
        (redraw,dpy,win) <- modifyMVar state $ \s -> do
          let s' = parseCmd c s
          return (s', (imgPath s' /= imgPath s, stDpy s', stWin s'))
        when redraw $ sendExposeEvent dpy win >> updateCache state

eventLoop state = do
  s <- readMVar state
  img <- maybe (return Nothing) (getImg state) (imgPath s)
  drawImg (stDpy s) (stWin s) img
  X.allocaXEvent $ \e -> X.nextEvent (stDpy s) e
  eventLoop state

getImg state p = do
  State { stDpy = d, stImgCache = c, stLoadLock = l } <- readMVar state
  img <- maybe (withMVar l (\_ -> loadXImg d p)) (return . Just) (lookup p c)
  flip (maybe (return Nothing)) img $ \i -> modifyMVar state $ \s -> do
    let c' = (p,i) : (take (cacheSize-1) $ filter ((/=) p . fst) (stImgCache s))
        cacheSize = stImgCacheSize s
    return (s {stImgCache = c'}, img)

updateCache st = do
  s@(State {stIdx = idx, stPlaylist = pl}) <- readMVar st
  let idxs = take 5 [max 0 (idx-2) .. length pl - 1]
  forkIO $ sequence_ $ map (getImg st . (pl !!)) idxs
  return ()

parseCmd :: String -> State -> State
parseCmd cmd s@(State {stIdx = idx, stPlaylist = pl}) = case words cmd of
  ["next"] -> gotoIdx s (idx+1)
  ["prev"] -> gotoIdx s (idx-1)
  ["first"] -> gotoIdx s 0
  ["last"] -> gotoIdx s (length pl - 1)
  "playlist":"add":imgs -> s { stPlaylist = pl++imgs }
  "playlist":"replace":imgs ->
    s { stIdx = 0, stPlaylist = imgs, stImgCache = [] }
  "playlist":"insert":"0":imgs ->
    s { stIdx = idx + (length imgs), stPlaylist = imgs++pl }
  _ -> s

gotoIdx s@(State {stPlaylist = pl}) n
  | n >= 0 && n < (length pl) = s { stIdx = n }
  | otherwise = s
