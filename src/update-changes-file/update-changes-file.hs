{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import UpdateChangesFile
import MyCabal

import Control.Exception ( SomeException(..), catch, displayException, fromException, throwIO )
import Data.Maybe
import Data.Optional ( Optional )
import qualified Data.Text as Text
import Prelude hiding ( FilePath )
import System.Environment ( getProgName )
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )
import Turtle hiding ( stdout, stderr )

type Options = (Maybe TimeStamp, FilePath, PackageName, Version, Revision, EMail)

parser :: Parser Options
parser = do
  now <- optional (optText "timestamp" 't' "timestamp for the generated change log entry")
  changesFile <- argPath "changes-file" "package change log file to update"
  pkg <- argParsec "package-name" "name of the Hackage package"
  vers <- argParsec "new-version" "version string of the updated package"
  rv <- argInt "new-revision" "Hackage revision of the updated package"
  email <- argText "e-mail" "e-mail address of the revision author"
  pure (now, changesFile, pkg, vers, rv, email)

main :: IO ()
main = run `catch` reportError

-- | Report uncaught exceptions as @prog: message@ like GHC < 9.10 did, i.e.
-- without the exception type header and the backtrace that newer versions of
-- the default top-level handler add.
reportError :: SomeException -> IO ()
reportError ex@(SomeException ex')
  | Just ec <- fromException ex = throwIO (ec :: ExitCode)
  | otherwise = do prog <- getProgName
                   hPutStrLn stderr (prog ++ ": " ++ displayException ex')
                   exitFailure

run :: IO ()
run = do
  (now,changesFile,pkg,newv,rv,email) <- options "Guess the change log entry between two version of a package." parser
  updateChangesFile now (Text.unpack (format fp changesFile)) pkg (newv,rv) email

argParsec :: Parsec a => ArgName -> Optional HelpMessage -> Parser a
argParsec = arg (simpleParsec . Text.unpack)
