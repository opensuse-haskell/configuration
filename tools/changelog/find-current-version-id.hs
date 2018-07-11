module Main ( main ) where

import ExtractVersionUpdates

import Distribution.Pretty
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [p] -> do vs <- extractVersionUpdates p
              if (null vs)
                 then die ("*** cannot recognize any updates in " ++ p)
                 else putStrLn (prettyShow (head vs))
    _   -> syntaxError

syntaxError :: IO ()
syntaxError = die "Usage: find-current-version-id <changes-file>"
