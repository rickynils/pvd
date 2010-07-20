-- vim: syntax=haskell

module Main (
  main
) where

import System (getArgs)
import System.Console.GetOpt
import Control.Monad (when)
import qualified Data.Map as Map

data Flag =
  FileList String | Port String | Add | Insert | Replace

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
  ]

commands =
  [ Command "playlist" playlistOpts playlistAct playlistUsage
  ]

commandMap = Map.fromList $ map (\c -> (cmdStr c, c)) commands

playlistOpts =
  [ Option ['l'] ["filelist"] (ReqArg FileList "FILE") "a file that contains paths of the photos that should be added to the playlist"
  , Option ['a'] ["add"] (NoArg Add) "adds the selected files to the end of the current playlist"
  , Option ['r'] ["replace"] (NoArg Replace) "replaces the contents of the current playlist with the selected files"
  , Option ['i'] ["insert"] (NoArg Insert) "inserts the selected files first in the current playlist"
  ]

playlistAct _ _ = return ()

playlistUsage = "[OPTIONS] [FILES]\n\n  Manages the current pvd playlist"
