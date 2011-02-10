module PvdSocket (
  initSocket,
  handleClient
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (when)
import Network.Socket
import qualified System.IO as IO
import qualified XUtils as X
import PvdState

handleClient s = accept (stSocket s) >>= forkIO . (uncurry processMessages)
  where processMessages connsock clientaddr = do
          connhdl <- socketToHandle connsock IO.ReadMode
          IO.hSetBuffering connhdl IO.LineBuffering
          messages <- IO.hGetContents connhdl
          redraw <- fmap or $ mapM (atomically . handleCmd s) (lines messages)
          IO.hClose connhdl
          when redraw $ X.sendExposeEvent (stDpy s) (stWin s)

initSocket port = withSocketsDo $ do
  addrinfos <- getAddrInfo
    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
    Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 5
  return sock

handleCmd s@(State {stIdx = idx, stPlaylist = pl}) cmd = case words cmd of
  ["next"] -> gotoIdx s (+ 1)
  ["prev"] -> gotoIdx s (+ (-1))
  ["first"] -> gotoIdx s (\_ -> 0)
  ["last"] -> readTVar pl >>= (\p -> gotoIdx s (\_ -> length p - 1))
  "playlist":"add":imgs -> do
    fmap (++ imgs) (readTVar pl) >>= writeTVar pl
    return True
  "playlist":"replace":imgs -> do
    writeTVar idx 0
    writeTVar pl imgs
    return True
  "playlist":"insert":"0":imgs -> do
    p <- readTVar pl
    fmap (+ length p) (readTVar idx) >>= writeTVar idx
    writeTVar pl (imgs++p)
    return True
  _ -> return False

gotoIdx (State {stIdx = idx, stPlaylist = pl}) f = do
  i <- readTVar idx
  let i' = f i
  l <- fmap length (readTVar pl)
  when (i' /= i && i' >= 0 && i' < l) (writeTVar idx i')
  return (i' /= i)
