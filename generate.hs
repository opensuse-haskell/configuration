module Main where

import Distribution.Hackage.DB
import Distribution.Text
import Distribution.Version
import Data.Maybe

main :: IO ()
main = do
  hackage <- readHackage
  stackage <- parseCabalConfig <$> readFile "cabal.config"
  mapM_ (putStrLn . toMakefile) stackage

toMakefile :: PackageIdentifier -> String
toMakefile p@(PackageIdentifier n v) =
  let pn = display n
      pv = display v
      pid = display p
      dir = "ghc-" ++ pn
      spec = dir ++ "/ghc-" ++ pn ++ ".spec"
      tar = dir ++ "/" ++ pid ++ ".tar.gz"
  in
    "all:: " ++ spec ++ " " ++ tar ++ "\n\
    \\n\
    \" ++ tar ++ ":\n\
    \\tmkdir -p " ++ dir ++ "\n\
    \\tcd " ++ dir ++ " && wget -q http://hackage.haskell.org/package/" ++ pid ++ "/" ++ pid ++ ".tar.gz\n\
    \\n\
    \" ++ spec ++ ":\n\
    \\tmkdir -p " ++ dir ++ "\n\
    \\tcd " ++ dir ++ " && rm -f *.spec && cblrpm spec " ++ pid ++ "\n\
    \\tspec-cleaner -i $@\n"

parseCabalConfig :: String -> [PackageIdentifier]
parseCabalConfig buf = dependencyToId <$> catMaybes (parseCabalConfigLine <$> lines buf)

parseCabalConfigLine :: String -> Maybe Dependency
parseCabalConfigLine ('-':'-':_) = Nothing
parseCabalConfigLine ('c':'o':'n':'s':'t':'r':'a':'i':'n':'t':'s':':':l) = parseCabalConfigLine l
parseCabalConfigLine (' ':l) = parseCabalConfigLine l
parseCabalConfigLine l = simpleParse (if last l == ',' then init l else l)

dependencyToId :: Dependency -> PackageIdentifier
dependencyToId d@(Dependency n vr) = PackageIdentifier n v
  where v   = fromMaybe err (isSpecificVersion vr)
        err = error ("dependencyToId: unexpected argument " ++ show d)
