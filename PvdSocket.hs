module PvdSocket (
  initSocket,
  handleClient
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (liftM, liftM2, liftM3, when)
import Network.Socket
import qualified System.IO as IO
import qualified XUtils as X
import PvdMonad

handleClient conf = do
  (socket,dpy,win) <- runPvd conf (liftM3 (,,) getSocket getDpy getWin)
  accept socket >>= forkIO . uncurry (processMessages dpy win)
  where processMessages dpy win connsock clientaddr = do
          connhdl <- socketToHandle connsock IO.ReadMode
          IO.hSetBuffering connhdl IO.LineBuffering
          messages <- IO.hGetContents connhdl
          redraw <- fmap or $ mapM (runPvd conf . handleCmd) (lines messages)
          IO.hClose connhdl
          when redraw $ X.sendExposeEvent dpy win

initSocket port = withSocketsDo $ do
  addrinfos <- getAddrInfo
    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
    Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 5
  return sock

handleCmd :: String -> Pvd Bool
handleCmd cmd = case words cmd of
  ["next"] -> modIdx $ (1 +) . snd
  ["prev"] -> modIdx $ (-1 +) . snd
  ["last"] -> modIdx $ (-1 +) . fst
  ["first"] -> modIdx $ \_ -> 0
  "playlist":"add":imgs -> modPlaylist (++ imgs)
  "playlist":"replace":imgs -> liftM2 (||) (setIdx 0) (setPlaylist imgs)
  "playlist":"insert":"0":imgs ->
    liftM2 (||) (modPlaylist (imgs ++)) (modIdx $ (length imgs +) . snd)
  _ -> return False
