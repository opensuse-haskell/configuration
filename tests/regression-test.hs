module Main ( main ) where

import ExtractVersionUpdates

import Control.Monad
import Data.Maybe
import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Version

main :: IO ()
main = forM_ tests $ \(txt,v') -> runTest v' txt

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

(~~>) :: String -> String -> (String, Version)
(~~>) txt v = (txt, fromMaybe (error ("invalid version: " ++ show v)) (simpleParsec v))
