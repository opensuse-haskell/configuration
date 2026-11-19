{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Prelude hiding ( fail )

import ExtractVersionUpdates
import MyCabal
import UpdateChangesFile

import Control.Monad
import Data.Maybe
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Time.Calendar ( fromGregorian )
import Data.Time.Clock ( UTCTime(..), secondsToDiffTime )

main :: IO ()
main = do
  forM_ tests $ \(txt,v') -> runTest v' txt
  timeStampTests
  entryTextTests

-- * Recognizing version updates in change log entries

runTest :: MonadFail m => (Version, Revision) -> String -> m ()
runTest v' txt = case isVersionUpdate txt of
  Nothing -> fail (unwords ["line", show txt, "failed to parse; should give", prettyVersion v'])
  Just v  -> unless (v == v') $
               fail (unwords ["line", show txt, "parsed to", prettyVersion v, "when", prettyVersion v', "was expected"])

tests :: [(String, (Version, Revision))]
tests =
  [ "- Update texmath to version 0.11.0.1."                     ~~> ("0.11.0.1", 0)
  , "- Update to version 0.9.4.1."                              ~~> ("0.9.4.1", 0)
  , "- Update to version 0.9.4 with cabal2obs."                 ~~> ("0.9.4", 0)
  , "- Update to version 0.8.6.5 revision 0 with cabal2obs."    ~~> ("0.8.6.5", 0)
  , "- update to 0.8.6.4"                                       ~~> ("0.8.6.4", 0)
  , "- update to 0.6.1.5 from upstream"                         ~~> ("0.6.1.5", 0)
  , "- upgrade to 0.6.0.3 from upstream (for pandoc 1.9.1.2)"   ~~> ("0.6.0.3", 0)
  , "- Add unliftio-core at version 0.1.1.0."                   ~~> ("0.1.1.0", 0)
  , "- Adding initial version version 0.1.1.0."                 ~~> ("0.1.1.0", 0)
  , "- Add ghc-paths at version 0.1.0.12 revision 1."           ~~> ("0.1.0.12", 1)
  ]

(~~>) :: String -> (String, Revision) -> (String, (Version, Revision))
(~~>) txt (v,r) = (txt, (fromMaybe (error ("invalid version: " ++ show v)) (simpleParsec v), r))

prettyVersion :: (Version, Revision) -> String
prettyVersion (v,r) = "version " <> prettyShow v <> " revision " <> show r

-- * Time stamps of change log entries

timeStampTests :: IO ()
timeStampTests = do
  check "parse time stamp with single-digit day"
    (parseTimeStamp "Mon Mar  9 11:47:45 UTC 2026") (Just (utc 2026 3 9 11 47 45))
  check "parse time stamp with two-digit day"
    (parseTimeStamp "Thu Nov 19 08:00:00 UTC 2026") (Just (utc 2026 11 19 8 0 0))
  check "reject 12-hour time stamp"
    (parseTimeStamp "Thu Feb 10 10:24:16 PM UTC 2022") Nothing

  check "latest time stamp of empty change log"
    (latestTimeStamp "") (Right Nothing)
  check "latest time stamp is taken from the topmost entry"
    (latestTimeStamp (changeLog ["Mon Mar  9 18:37:53 UTC 2026", "Mon Mar  4 08:50:57 UTC 2024"]))
    (Right (Just (utc 2026 3 9 18 37 53)))
  check "latest time stamp in change log with CRLF line endings"
    (latestTimeStamp (Text.replace "\n" "\r\n" (changeLog ["Mon Mar  9 18:37:53 UTC 2026"])))
    (Right (Just (utc 2026 3 9 18 37 53)))
  check "unparsable latest time stamp is an error"
    (latestTimeStamp (changeLog ["Thu Feb 10 10:24:16 PM UTC 2022"]))
    (Left "cannot parse time stamp of the latest entry: Thu Feb 10 10:24:16 PM UTC 2022 - joe@example.org")

  let t = utc 2026 3 9 11 47 45
  check "next time stamp of first entry"
    (nextTimeStamp t Nothing) t
  check "next time stamp after older entry"
    (nextTimeStamp t (Just (utc 2024 1 1 0 0 0))) t
  check "next time stamp after newer entry"
    (nextTimeStamp t (Just (utc 2026 3 9 18 37 53))) (utc 2026 3 9 18 37 54)
  check "next time stamp after entry with identical time stamp"
    (nextTimeStamp t (Just t)) (utc 2026 3 9 11 47 46)
  where
    changeLog :: [Text] -> Text
    changeLog ts = Text.concat
      [ "-------------------------------------------------------------------\n" <> ts' <> " - joe@example.org\n\n- Something.\n\n" | ts' <- ts ]

utc :: Integer -> Int -> Int -> Integer -> Integer -> Integer -> UTCTime
utc y m d hh mm ss = UTCTime (fromGregorian y m d) (secondsToDiffTime (hh * 3600 + mm * 60 + ss))

-- * Body text of change log entries

entryTextTests :: IO ()
entryTextTests = do
  check "indent entry text"
    (indentEntryText "## 1.0\n\n* Something.\n")
    "  ## 1.0\n\n  * Something.\n"
  check "strip trailing whitespace from entry text"
    (indentEntryText "## 1.0 \t\n   \n* Something.  \n")
    "  ## 1.0\n\n  * Something.\n"
  check "keep leading whitespace in entry text"
    (indentEntryText "* Something\n  that continues.\n")
    "  * Something\n    that continues.\n"

-- * Utilities

check :: (Eq a, Show a) => String -> a -> a -> IO ()
check name got expected =
  unless (got == expected) $
    fail (unwords [name ++ ":", "got", show got, "but expected", show expected])
