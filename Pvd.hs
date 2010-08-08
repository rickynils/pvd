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
import qualified Data.Map as M
import System.Exit (exitWith, ExitCode(..))
import System.IO
import XUtils

data State = State {
  stIdx :: Int,
  stPlaylist :: [String],
  stDpy :: X.Display,
  stWin :: X.Window,
  stImgCache :: M.Map String XImg,
  stLoadLock :: MVar ()
}

stImg (State {stIdx = idx, stPlaylist = pl})
  | idx >= 0 && idx < length pl = Just (pl !! idx)
  | otherwise = Nothing

main = do
  IL.ilInit
  (dpy,win) <- initX
  l <- newMVar ()
  state <- newMVar $ State {
    stIdx = -1, stPlaylist = [], stDpy = dpy, stWin = win, stImgCache = M.empty,
    stLoadLock = l
  }
  forkIO $ eventLoop state
  initSocket "4245" >>= socketLoop state

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
          return (s', (stImg s' /= stImg s, stDpy s', stWin s'))
        when redraw $ sendExposeEvent dpy win >> updateCache state

eventLoop state = do
  s <- readMVar state
  img <- maybe (return Nothing) (getImg state) (stImg s)
  drawImg (stDpy s) (stWin s) img
  X.allocaXEvent $ \e -> X.nextEvent (stDpy s) e
  eventLoop state

getImg state p = do
  State { stDpy = d, stImgCache = c, stLoadLock = l } <- readMVar state
  img <- maybe (withMVar l (\_ -> loadXImg d p)) (return . Just) (M.lookup p c)
  flip (maybe (return Nothing)) img $ \i -> modifyMVar state $ \s ->
    return (s {stImgCache = M.insert p i (stImgCache s)}, img)

updateCache st = do
  s@(State {stIdx = idx, stPlaylist = pl}) <- readMVar st
  forkIO $ sequence_ $ map (getImg st . (pl !!)) $ take 4 [idx .. length pl - 1]
  return ()

parseCmd :: String -> State -> State
parseCmd cmd s@(State {stIdx = idx, stPlaylist = pl}) = case words cmd of
  ["next"] | idx+1 < length pl ->  s { stIdx = idx+1 }
  ["prev"] | idx-1 >= 0 ->  s { stIdx = idx-1 }
  "playlist":"add":imgs -> s { stPlaylist = pl++imgs }
  "playlist":"replace":imgs ->
    s { stIdx = 0, stPlaylist = imgs, stImgCache = M.empty }
  "playlist":"insert":"0":imgs ->
    s { stIdx = idx + (length imgs), stPlaylist = imgs++pl }
  _ -> s
