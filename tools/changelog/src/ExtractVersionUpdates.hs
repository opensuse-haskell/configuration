module ExtractVersionUpdates ( extractVersionUpdates, isVersionUpdate ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Distribution.Parsec.Class
import Distribution.Pretty
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

-- * Regression Tests

(~~>) :: String -> String -> (String, Version)
(~~>) txt v = (txt, fromMaybe (error ("invalid version: " ++ show v)) (simpleParsec v))

runTests :: IO ()
runTests = forM_ tests $ \(txt,v') -> runTest v' txt

runTest :: Monad m => Version -> String -> m ()
runTest v' txt = case isVersionUpdate txt of
  Nothing -> fail (unwords ["line", show txt, "failed to parse; should give", prettyShow v'])
  Just v  -> unless (v == v') $
               fail (unwords ["line", show txt, "parsed to", prettyShow v, "when", prettyShow v', "was expected"])

tests :: [(String, Version)]
tests =
  [ "- Update texmath to version 0.11.0.1."                     ~~> "0.11.0.1"
  , "- Update to version 0.9.4.1."                              ~~> "0.9.4.1"
  , "- Update to version 0.9.4 with cabal2obs."                 ~~> "0.9.4"
  , "- Update to version 0.8.6.5 revision 0 with cabal2obs."    ~~> "0.8.6.5"
  , "- update to 0.8.6.4"                                       ~~> "0.8.6.4"
  , "- update to 0.6.1.5 from upstream"                         ~~> "0.6.1.5"
  , "- upgrade to 0.6.0.3 from upstream (for pandoc 1.9.1.2)"   ~~> "0.6.0.3"
  , "- Add unliftio-core at version 0.1.1.0."                   ~~> "0.1.1.0"
  , "- Adding initial version version 0.1.1.0."                 ~~> "0.1.1.0"
  ]
