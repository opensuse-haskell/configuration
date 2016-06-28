module Main where

import Control.Monad
import Data.Maybe
import Distribution.Hackage.DB hiding ( null, map, filter )
import Distribution.Text
import Distribution.Version
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let cabalConfig | null args = "cabal.config"
                  | otherwise = head args
  hackage <- readHackage
  stackage <- parseCabalConfig <$> readFile cabalConfig
  let targets = flip map stackage $ \p@(PackageIdentifier (PackageName pn) v) ->
                  let dir = "$(OBSDIR)/" ++ (if isLibrary hackage p then "ghc-" else "") ++ pn
                  in  dir ++ "/" ++ display p ++ ".tar.gz"
  putStrLn $ unwords $ ["all:"] ++ targets ++ ["\n"]
  mapM_ (putStrLn . toMakefile hackage) stackage

isLibrary :: Hackage -> PackageIdentifier -> Bool
isLibrary hackage (PackageIdentifier (PackageName n) v) =
  isJust (condLibrary (hackage ! n ! v))

toMakefile :: Hackage -> PackageIdentifier -> String
toMakefile hackage p@(PackageIdentifier n v) =
  let pn = display n
      pv = display v
      pid = display p
      dir = "$(OBSDIR)/" ++ (if isLibrary hackage p then "ghc-" else "") ++ pn
      spec = dir ++ (if isLibrary hackage p then "/ghc-" else "/") ++ pn ++ ".spec"
      tar = dir ++ "/" ++ pid ++ ".tar.gz"
  in
    tar ++ " : " ++ spec ++ "\n\
    \\trm -f " ++ dir ++ "/*.tar.gz\n\
    \\tcp ~/.cabal/packages/hackage.haskell.org/" ++ pn ++ "/" ++ pv ++ "/" ++ pid ++ ".tar.gz " ++ dir ++"/\n\
    \\n\
    \" ++ spec ++ ":\n\
    \\tmkdir -p " ++ dir ++ "\n\
    \\tcd " ++ dir ++ " && rm -f *.spec && cblrpm spec " ++ pid ++ "\n\
    \\tspec-cleaner -i $@\n"

parseCabalConfig :: String -> [PackageIdentifier]
parseCabalConfig buf = filter cleanup $ dependencyToId <$> catMaybes (parseCabalConfigLine <$> lines buf)
  where
    cleanup :: PackageIdentifier -> Bool
    cleanup (PackageIdentifier (PackageName n) _) = n `notElem` (corePackages ++ bannedPackages)

parseCabalConfigLine :: String -> Maybe Dependency
parseCabalConfigLine ('-':'-':_) = Nothing
parseCabalConfigLine ('c':'o':'n':'s':'t':'r':'a':'i':'n':'t':'s':':':l) = parseCabalConfigLine l
parseCabalConfigLine (' ':l) = parseCabalConfigLine l
parseCabalConfigLine l = simpleParse (if last l == ',' then init l else l)

dependencyToId :: Dependency -> PackageIdentifier
dependencyToId d@(Dependency n vr) = PackageIdentifier n v
  where v   = fromMaybe err (isSpecificVersion vr)
        err = error ("dependencyToId: unexpected argument " ++ show d)

corePackages :: [String]
corePackages = [ "ghc"
               , "array"
               , "base"
               , "bin-package-db"
               , "binary"
               , "bytestring"
               , "Cabal"
               , "containers"
               , "deepseq"
               , "directory"
               , "filepath"
               , "ghc-prim"
               , "haskeline"
               , "hoopl"
               , "hpc"
               , "integer-gmp"
               , "pretty"
               , "process"
               , "template-haskell"
               , "time"
               , "transformers"
               , "unix"
               ]

bannedPackages :: [String]
bannedPackages = [ "bytestring-builder"
                 , "nats"
                 , "Win32"
                 , "Win32-extras"
                 , "Win32-notify"
                 ]
