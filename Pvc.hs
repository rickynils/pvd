-- vim: syntax=haskell

module Main (
  main
) where

import System (getArgs)
import System.Console.GetOpt
import Control.Monad (when)
import qualified Data.Map as Map
import Network.Socket
import Network.BSD
import System.IO

data Flag =
  Playlist String | Port String | Host String | Add | Insert | Replace
    deriving (Eq)

data Command = Command {
  cmdStr :: String,
  cmdOptions :: [OptDescr Flag],
  cmdAction :: [Flag] -> [String] -> IO (),
  cmdUsage :: String
}

main = do
  args <- getArgs
  when (null args) (usageError Nothing "No command given")
  (cmd, flags, files) <- parseOptions (head args) (tail args)
  cmd flags files

usageError Nothing msg = do
  putStrLn "Usage:\n  pvc COMMAND [OPTIONS]\n"
  putStrLn (usageInfo "Options valid for all commands:" commonOptions)
  putStrLn "Available commands:"
  putStrLn $ unlines $ map ("  "++) (Map.keys commandMap)
  fail msg

usageError (Just (Command cn opts _ usage)) msg = do
  putStrLn ("Usage:\n  pvc "++cn++" "++usage++"\n")
  putStrLn (usageInfo "Options valid for all commands:" commonOptions)
  putStrLn (usageInfo ("Options valid for command \""++cn++"\":") opts)
  fail msg

parseOptions cmd argv = case Map.lookup cmd commandMap of
  Nothing -> usageError Nothing $ "Unknown command: "++cmd
  Just c -> case getOpt Permute (commonOptions++(cmdOptions c)) argv of
    (o,n,[]  ) -> return (cmdAction c, o, n)
    (_,_,errs) -> usageError (Just c) (concat $ map (filter ('\n' /=)) errs)

commonOptions =
  [ Option ['p'] ["port"] (ReqArg Port "PORT") "photo viewer daemon port"
  , Option ['h'] ["host"] (ReqArg Host "HOST") "photo viewer daemon host"
  ]

commands =
  [ Command "playlist" playlistOpts playlistAct playlistUsage
  , Command "next" [] nextAct nextUsage
  , Command "prev" [] prevAct prevUsage
  , Command "first" [] firstAct firstUsage
  , Command "last" [] lastAct lastUsage
  ]

commandMap = Map.fromList $ map (\c -> (cmdStr c, c)) commands

sendCmdStr flags cmd = do
  addrinfos <- getAddrInfo Nothing (Just host) (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  h <- socketToHandle sock WriteMode
  hPutStrLn h cmd
  hClose h
    where
      host = last [h | Host h <- Host "127.0.0.1" : flags]
      port = last [p | Port p <- Port "4245" : flags]


playlistOpts =
  [ Option ['l'] ["filelist"] (ReqArg Playlist "PLAYLIST") "playlist file"
  , Option ['a'] ["add"] (NoArg Add) "adds the selected files to the end of the current playlist"
  , Option ['r'] ["replace"] (NoArg Replace) "replaces the contents of the current playlist with the selected files"
  , Option ['i'] ["insert"] (NoArg Insert) "inserts the selected files first in the current playlist"
  ]

playlistAct flags files1 = do
  files2 <- readPlaylist flags
  sendCmdStr flags $ cmdStr++" "++(unwords $ files1++files2)
  where
    cmdStr | elem Add flags = "playlist add"
           | elem Insert flags = "playlist insert 0"
           | otherwise = "playlist replace"
    readPlaylist fs = fmap (concat . map words) $ sequence [readFile pl | (Playlist pl) <- fs]

playlistUsage = "[OPTIONS] [FILES]\n\n  Manages the current pvd playlist"

nextAct flags _ = sendCmdStr flags "next"

nextUsage = "\n\n  Shows the next photo in the playlist"

prevAct flags _ = sendCmdStr flags "prev"

prevUsage = "\n\n  Shows the previous photo in the playlist"

firstAct flags _ = sendCmdStr flags "first"

firstUsage = "\n\n  Shows the first photo in the playlist"

lastAct flags _ = sendCmdStr flags "last"

lastUsage = "\n\n  Shows the last photo in the playlist"
