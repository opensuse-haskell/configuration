module ExtractVersionUpdates ( extractVersionUpdates, isVersionUpdate ) where

import Control.Applicative
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Distribution.Parsec.Class
import Distribution.Version
import Text.Regex.Posix

type SubmatchId = Int
type Pattern = String

extractVersionUpdates :: FilePath -> IO [Version]
extractVersionUpdates fp = do
  buf <- Text.readFile fp
  return [ v | l <- Text.lines buf, Just v <- [isVersionUpdate (Text.unpack l)] ]

isVersionUpdate :: String -> Maybe Version
isVersionUpdate l = foldl1 (<|>) [ tryMatch i patt l | (i,patt) <- formats ]
  where
    formats :: [(SubmatchId, Pattern)]
    formats =
      [ (1, "^-.* update .*version ([0-9.]+).$")
      , (1, "^-.* update .*version ([0-9.]+) (revision|with cabal2obs)")
      , (1, "^-.* update to ([0-9.]+)$")
      , (2, "^-.* (update|upgrade) to ([0-9.]+) from upstream")
      , (1, "^-.* add .*at version ([0-9.]+).$")
      , (1, "^-.* adding initial .*version ([0-9.]+).$")
      ]

-- * Utility Functions to Simplify Regular Expression Matching

tryMatch :: SubmatchId -> Pattern -> String -> Maybe Version
tryMatch i patt l = submatch i (getAllTextSubmatches (match (mkRegex patt) l)) >>= simpleParsec

submatch :: SubmatchId -> [String] -> Maybe String
submatch i xs
  | i < 0 || null xs = Nothing
  | i == 0           = Just (head xs)
  | otherwise        = submatch (i-1) (tail xs)

mkRegex :: Pattern -> Regex
mkRegex = makeRegexOpts (defaultCompOpt + compIgnoreCase) execBlank
