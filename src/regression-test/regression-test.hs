module Main ( main ) where

import Prelude hiding ( fail )

import ExtractVersionUpdates

import Control.Monad
import Data.Maybe
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Version

main :: IO ()
main = forM_ tests $ \(txt,v') -> runTest v' txt

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
  ]

(~~>) :: String -> (String, Revision) -> (String, (Version, Revision))
(~~>) txt (v,r) = (txt, (fromMaybe (error ("invalid version: " ++ show v)) (simpleParsec v), r))

prettyVersion :: (Version, Revision) -> String
prettyVersion = undefined
