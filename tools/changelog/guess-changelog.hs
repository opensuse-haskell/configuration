{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import GuessChangelog

import Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Prelude hiding ( FilePath )
import Turtle hiding ( l )

parser :: Parser (FilePath, FilePath)
parser = do
  old <- argPath "old" "directory containing the old package version"
  new <- argPath "new" "directory containing the new package version"
  pure (old,new)

main :: IO ()
main = do
  (oldDir,newDir) <- options "Guess the change log entry between two version of a package." parser
  result <- guessChangelog oldDir newDir
  case result of
    Right txt -> Text.putStrLn txt
    Left desc -> case desc of
      NoChangelogFiles            -> eprintf ("no change log files found\n")
      UndocumentedUpdate p        -> eprintf ("file "%fp%" has not changed between releases\n") p
      NoCommonChangelogFiles l r  -> eprintf ("both directories have no files in common: "%fps%" vs. "%fps%"\n") l r
      MoreThanOneChangelogFile p  -> eprintf ("too many changelog files: "%fps%"\n") p
      UnmodifiedTopIsTooLarge p n -> eprintf (fp%" has more than 10 unmodified lines at top: "%d%"\n") p n
      NotJustTopAdditions p       -> eprintf (fp%" has more edits than just adding at the top\n") p

-- * Utility Functions

fps :: Format r (Set FilePath -> r)
fps = makeFormat (Text.intercalate ", " . map (format fp) . Set.toAscList)
