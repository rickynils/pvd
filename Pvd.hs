module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (when, forever, liftM3, liftM)
import Data.Maybe (maybe)
import qualified Codec.Image.DevIL as IL (ilInit)
import qualified Graphics.X11.Xlib as X
import qualified Graphics.X11.Xlib.Extras as X
import qualified XUtils as X
import System.Console.GetOpt
import System.Exit (exitSuccess)
import System (getArgs)
import PvdSocket (initSocket, handleClient)
import PvdMonad

data Flag =
  Port String | CacheSize Int | Playlist String | Help
    deriving (Eq)

main = do
  conf <- initApp
  forkIO $ forever $ updateCache conf
  forkIO $ forever $ handleEvent conf
  forever $ handleClient conf

initApp = do
  args <- getArgs
  (flags, fs1) <- parseOptions args
  fs2 <- fmap (concatMap words) $ sequence [readFile p | (Playlist p) <- flags]
  let cacheSize = last [c | CacheSize c <- CacheSize 15 : flags]
      port = last [p | Port p <- Port "4245" : flags]
      playlist = fs1++fs2
  IL.ilInit
  (dpy,win) <- X.initX
  socket <- initSocket port
  initPvd playlist dpy win cacheSize socket

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

handleEvent conf = do
  (img, dpy, win) <- runPvd conf (liftM3 (,,) currentImage getDpy getWin)
  X.drawImg dpy win img
  X.allocaXEvent $ \e -> X.nextEvent dpy e

updateCache conf = do
  (path, dpy) <- runPvd conf $ do
    path <- fetchNextPath
    putImgInCache LoadingImg path
    liftM ((,) path) getDpy
  img <- fmap (maybe LoadFailed CachedImg) (X.loadXImg dpy path)
  runPvd conf (putImgInCache img path)
