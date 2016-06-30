module Main where

import Control.Exception ( assert )
import Control.Monad
import Data.Maybe
import Distribution.Hackage.DB hiding ( null, map, filter, lookup )
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
                  let forcedExe = pn `elem` forcedExecutablePackages
                      isExe = forcedExe || not (isLibrary hackage p)
                      dir = "$(OBSDIR)/" ++ (if isExe then "" else "ghc-") ++ pn
                  in  dir ++ "/" ++ display p ++ ".tar.gz"
  putStrLn $ unwords $ ["all:"] ++ targets
  mapM_ (putStr . toMakefile hackage) stackage

hackageRevision :: Hackage -> PackageIdentifier -> Int
hackageRevision hackage (PackageIdentifier (PackageName n) v) =
  maybe 0 read (lookup "x-revision" (customFieldsPD (packageDescription (hackage ! n ! v))))

isLibrary :: Hackage -> PackageIdentifier -> Bool
isLibrary hackage (PackageIdentifier (PackageName n) v) =
  isJust (condLibrary (hackage ! n ! v))

toMakefile :: Hackage -> PackageIdentifier -> String
toMakefile hackage p@(PackageIdentifier n v) =
  let pn = display n
      pv = display v
      r = hackageRevision hackage p
      pid = display p
      forcedExe = unPackageName n `elem` forcedExecutablePackages
      isExe = forcedExe || not (isLibrary hackage p)
      dir = "$(OBSDIR)/" ++ (if isExe then "" else "ghc-") ++ pn
      spec = dir ++ (if isExe then "/" else "/ghc-") ++ pn ++ ".spec"
      tarsrc = "~/.cabal/packages/hackage.haskell.org/" ++ pn ++ "/" ++ pv ++ "/" ++ pid ++ ".tar.gz"
      tar = dir ++ "/" ++ pid ++ ".tar.gz"
      cbl = dir ++ "/" ++ show r ++ ".cabal"
      cblurl = "https://hackage.haskell.org/package/" ++ pid ++ "/revision/" ++ show r ++ ".cabal"
  in
    "\n" ++
    tar ++ " : " ++ spec ++ (if r > 0 then " " ++ cbl else "") ++ "\n\
    \\trm -f " ++ dir ++ "/*.tar.gz\n\
    \\tcp " ++ tarsrc ++ " " ++ dir ++"/\n\
    \\n\
    \" ++ spec ++ ":\n\
    \\tmkdir -p " ++ dir ++ "\n\
    \\tcd " ++ dir ++ " && rm -f *.spec && cblrpm " ++ (if forcedExe then "-b " else " ") ++ "spec " ++ pid ++ "\n\
    \\tspec-cleaner -i $@\n\
    \\tshopt -s nullglob && cd " ++ dir ++ " && for n in ../../" ++ pn ++ "-*.patch; do patch <$$n; done\n\
    \\n\
    \" ++ dir ++ "/" ++ show r ++ ".cabal:\n\
    \\tcd " ++ dir ++ " && rm -f *.cabal && wget -q " ++ cblurl ++ "\n"

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
corePackages =
  [ "ghc"
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
bannedPackages =
  [ "bytestring-builder"
  , "nats"
  , "hfsevents"
  , "Win32"
  , "Win32-extras"
  , "Win32-notify"
  ]

forcedExecutablePackages :: [String]
forcedExecutablePackages =
  [ "Agda"
  , "alex"
  , "BNFC"
  , "bustle"
  , "c2hs"
  , "cab"
  , "cabal-install"
  , "cabal-rpm"
  , "cpphs"
  , "darcs"
  , "ghc-mod"
  , "git-annex"
  , "gtk2hs-buildtools"
  , "happy"
  , "hdevtools"
  , "highlighting-kate"
  , "hindent"
  , "hlint"
  , "hpack"
  , "hscolour"
  , "idris"
  , "lhs2tex"
  , "pandoc"
  , "pointfree"
  , "pointful"
  , "shake"
  , "ShellCheck"
  , "stack"
  , "texmath"
  , "xmobar"
  , "xmonad"
  ]
