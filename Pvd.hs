-- vim: syntax=haskell

module Main (
  main
) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Data.Foldable (notElem)
import Data.Maybe
import Network.Socket
import Prelude hiding (notElem)
import qualified Codec.Image.DevIL as IL (ilInit)
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as X
import System.Exit (exitWith, ExitCode(..))
import System.IO
import XUtils

data State = State {
  stIdx :: Int,
  stPlaylist :: [String],
  stDpy :: X.Display,
  stWin :: X.Window
}

stImg (State {stIdx = idx, stPlaylist = pl})
  | idx >= 0 && idx < length pl = Just (pl !! idx)
  | otherwise = Nothing

main = do
  IL.ilInit
  (dpy,win) <- initX
  state <- newMVar $ State {
    stIdx = -1, stPlaylist = [], stDpy = dpy, stWin = win
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
  forkIO $ processMessages state connsock clientaddr
  socketLoop state sock
    where
      processMessages state connsock clientaddr = do
        connhdl <- socketToHandle connsock ReadMode
        hSetBuffering connhdl LineBuffering
        messages <- hGetContents connhdl
        mapM_ (runParseCmd state) (lines messages)
        hClose connhdl
      runParseCmd state c = do
        (redraw,dpy,win) <- modifyMVar state $ \s -> do
          let s' = parseCmd c s
          return (s', (stImg s' /= stImg s, stDpy s', stWin s'))
        when redraw $ sendExposeEvent dpy win

eventLoop state = runErrorT (innerLoop Nothing Nothing) >> return ()
  where
    gNotElem e m = guard $ notElem e m
    innerLoop path ximg = do
      s <- lift $ readMVar state
      let path' = stImg s
          dpy = stDpy s
      ximg' <- flip mplus (return ximg)
        (do Just p <- return path; gNotElem p path; fmap Just $ loadXImg dpy p)
      lift $ drawImg dpy (stWin s) ximg'
      lift $ X.allocaXEvent $ \e -> X.nextEvent dpy e
      innerLoop path' ximg'

parseCmd :: String -> State -> State
parseCmd _ s = s
