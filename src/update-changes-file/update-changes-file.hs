{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import UpdateChangesFile

import Data.Maybe
import Data.Optional ( Optional )
import qualified Data.Text as Text
import Distribution.Package
import Distribution.Parsec
import Distribution.Version
import Prelude hiding ( FilePath )
import Turtle hiding ( stdout, stderr )

type Options = (Maybe TimeStamp, FilePath, PackageName, Version, EMail)

parser :: Parser Options
parser = do
  now <- optional (optText "timestamp" 't' "timestamp for the generated change log entry")
  changesFile <- argPath "changes-file" "package change log file to update"
  pkg <- argParsec "package-name" "name of the Hackage package"
  vers <- argParsec "new-version" "version string of the updated package"
  email <- argText "e-mail" "e-mail address of the revision author"
  pure (now, changesFile, pkg, vers, email)

main :: IO ()
main = do
  (now,changesFile,pkg,newv,email) <- options "Guess the change log entry between two version of a package." parser
  updateChangesFile now (Text.unpack (format fp changesFile)) pkg newv email

argParsec :: Parsec a => ArgName -> Optional HelpMessage -> Parser a
argParsec = arg (simpleParsec . Text.unpack)
