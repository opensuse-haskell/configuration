module Main where

import Control.Exception ( assert )
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Distribution.Hackage.DB hiding ( null, map, filter, lookup )
import Distribution.Text
import Distribution.Version
import System.Directory
import System.Environment

main :: IO ()
main = do
  let buildDir = "_build"
  homeDir <- System.Environment.getEnv "HOME"
  hackage <- readHackage
  let stackageVersions = ["lts-6","nightly"]
  packageSets <- forM stackageVersions $ \stackageVersion -> do
    let cabalConfig = stackageVersion </> "config" </> "stackage-packages.txt"
    parseCabalConfig <$> readFile cabalConfig

  shakeArgs shakeOptions {shakeFiles=buildDir, shakeProgress=progressSimple} $ do

    forM_ (nub (concat packageSets)) $ \p@(PackageIdentifier (PackageName pn) v) -> do
      let pv = display v
          pid = display p
          tarsrc = homeDir </> ".cabal/packages/hackage.haskell.org" </> pn </> pv </> pid <.> "tar.gz"
      tarsrc %> \out -> do
        b <- liftIO $ System.Directory.doesFileExist out
        when (not b) $
          command_ [] "cabal" ["fetch", "-v0", "--no-dependencies", "--", display p]

    forM_ (zip stackageVersions packageSets) $ \(stackageVersion,stackage) -> do
      forM_ stackage $ \p@(PackageIdentifier (PackageName pn) v) -> do
        let forcedExe = pn `elem` forcedExecutablePackages
            isExe = forcedExe || not (isLibrary hackage p)
            rv = hackageRevision hackage p
            pkgName = (if isExe then "" else "ghc-") ++ pn
            pkgDir = "_build" </> stackageVersion </> pkgName
            spec = pkgDir </> pkgName <.> "spec"
            pv = display v
            pid = display p
            tar = pkgDir </> pid <.> "tar.gz"
            tarsrc = homeDir </> ".cabal/packages/hackage.haskell.org" </> pn </> pv </> pid <.> "tar.gz"
            editedCabalFile = pkgDir </> show rv <.> "cabal"
            editedCabalFileUrl = "https://hackage.haskell.org/package/" ++ pid ++ "/revision/" ++ show rv ++ ".cabal"

        want [spec, tar]

        tar %> \out -> do
          b <- liftIO $ System.Directory.doesFileExist tar
          when (not b) $ do
            bash [ "rm -f " ++ pkgDir ++ "/*.tar.gz", "cp " ++ tarsrc ++ " " ++ out ]

        when (rv > 0) $ do
          want [editedCabalFile]
          editedCabalFile %> \_ ->
            bash ["cd " ++ pkgDir, "rm -f *.cabal", "wget -q " ++ editedCabalFileUrl]

        spec %> \out -> do
          compiler <- stripSpaces <$> readFile' (stackageVersion </> "config" </> "compiler")
          patches <- sort <$> getDirectoryFiles pkgDir ["../../../" ++ stackageVersion ++ "/patches/" ++ pn ++ "/*.patch"]
          bash $ [ "cd " ++ pkgDir
                 , "rm -f *.spec"
                 , "../../../tools/cabal-rpm/dist/build/cabal-rpm/cabal-rpm --compiler=" ++
                   compiler ++ (if forcedExe then " -b " else " ") ++ "spec " ++ pid ++ " >/dev/null"
                 , "spec-cleaner -i " ++ pkgName <.> "spec"
                 ] ++
                 [ "patch --no-backup-if-mismatch --force <" ++ p | p <- patches ]

bash :: [String] -> Action ()
bash cmds = command_ [] "bash" ["-c", intercalate "; " cmds']
  where
    cmds' = "set -eu -o pipefail" : "shopt -s nullglob" : cmds

stripSpaces :: String -> String
stripSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

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
      tarsrc = "$(HOME)/.cabal/packages/hackage.haskell.org/" ++ pn ++ "/" ++ pv ++ "/" ++ pid ++ ".tar.gz"
      tar = dir ++ "/" ++ pid ++ ".tar.gz"
      cbl = dir ++ "/" ++ show r ++ ".cabal"
      cblurl = "https://hackage.haskell.org/package/" ++ pid ++ "/revision/" ++ show r ++ ".cabal"
  in unlines $
  [ ""
  , unwords [tar, ":", tarsrc]
  , unwords ["\t", "mkdir", "-p", dir]
  , unwords ["\t", "rm", "-f", dir ++ "/*.tar.gz"]
  , unwords ["\t", "cp", "$<", "$@"]
  , ""
  , unwords [tarsrc ++ ":"]
  , unwords ["\t", "cabal", "fetch", "-v0", "--no-dependencies", "--", pid]
  ]
  -- "\n\
  -- \" ++ tar ++ " : " ++ tarsrc ++ " \n\
  -- \\trm -f " ++ dir ++ "/*.tar.gz\n\
  -- \\tcp " ++ tarsrc ++ " " ++ dir ++"/\n\
  -- \\n\
  -- \" ++ spec ++ ": " ++ tar ++ (if r > 0 then " " ++ cbl else "") ++ "\n\
  -- \\tmkdir -p " ++ dir ++ "\n\
  -- \\tcd " ++ dir ++ " && rm -f *.spec && cabal-rpm --compiler=ghc-7.10 " ++ (if forcedExe then "-b " else " ") ++ "spec " ++ pid ++ "\n\
  -- \\tspec-cleaner -i $@\n\
  -- \\tshopt -s nullglob && set -e && cd " ++ dir ++ " && for n in $(SRCDIR)/$(VERSION)/patches/" ++ pn ++ "-*.patch; do patch <$$n; done\n\
  -- \\n\
  -- \" ++ dir ++ "/" ++ show r ++ ".cabal:\n\
  -- \\tmkdir -p " ++ dir ++ " && cd " ++ dir ++ " && rm -f *.cabal && wget -q " ++ cblurl ++ "\n"

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
  , "gtk"
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
