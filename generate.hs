module Main where

import Control.Monad
import Data.Maybe
import Distribution.Hackage.DB
import Distribution.Text
import Distribution.Version

main :: IO ()
main = do
  hackage <- readHackage
  stackage <- parseCabalConfig <$> readFile "cabal.config"
  forM_ stackage $ \p@(PackageIdentifier (PackageName n) v) -> do
    let pn = n
        pv = display v
        pid = display p
        dir = (if isLibrary hackage p then "ghc-" else "") ++ pn
        spec = dir ++ (if isLibrary hackage p then "/ghc-" else "/") ++ pn ++ ".spec"
        tar = dir ++ "/" ++ pid ++ ".tar.gz"
    putStrLn (unwords ["all::", spec, tar])
  putStrLn ""
  mapM_ (putStrLn . toMakefile hackage) stackage

isLibrary :: Hackage -> PackageIdentifier -> Bool
isLibrary hackage (PackageIdentifier (PackageName n) v) =
  isJust (condLibrary (hackage ! n ! v))

toMakefile :: Hackage -> PackageIdentifier -> String
toMakefile hackage p@(PackageIdentifier n v) =
  let pn = display n
      pv = display v
      pid = display p
      dir = (if isLibrary hackage p then "ghc-" else "") ++ pn
      spec = dir ++ (if isLibrary hackage p then "/ghc-" else "/") ++ pn ++ ".spec"
      tar = dir ++ "/" ++ pid ++ ".tar.gz"
  in
    tar ++ ":\n\
    \\tmkdir -p " ++ dir ++ "\n\
    \\trm -f " ++ dir ++ "/*.tar.gz\n\
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
