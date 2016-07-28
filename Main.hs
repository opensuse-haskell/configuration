{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main ( main ) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import qualified Distribution.Hackage.DB as DB
import Distribution.Hackage.DB hiding ( null, map, filter, lookup, pkgName )
import Distribution.Text
import Distribution.Version
import GHC.Generics ( Generic )
import System.Directory
import System.Environment
import Orphans ()
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype StackageVersion = StackageVersion String deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

data SusePackageDescription = SusePackageDescription
  { _rv :: Int
  , _isExe :: Bool
  }
  deriving (Show,Typeable,Eq,Generic)

instance Hashable SusePackageDescription
instance Binary SusePackageDescription
instance NFData SusePackageDescription

main :: IO ()
main = do
  let buildDir = "_build"
  homeDir <- System.Environment.getEnv "HOME"
  hackage <- readHackage
  let stackageVersions = ["lts-6","nightly"]
  packageSets <- forM stackageVersions $ \stackageVersion -> do
    let cabalConfig = "config" </> stackageVersion </> "stackage-packages.txt"
        extraConfig = "config" </> stackageVersion </> "extra-packages.txt"
    ps1 <- parseCabalConfig <$> readFile cabalConfig
    ps2 <- parseExtraConfig hackage <$> readFile extraConfig
    return (ps1 ++ ps2)

  shakeArgs shakeOptions {shakeFiles=buildDir, shakeProgress=progressSimple} $ do

    getSusePkgDescription <- addOracle $ \p@(PackageIdentifier (PackageName pn) _) -> do
      let forcedExe = pn `elem` forcedExecutablePackages
          isExe = forcedExe || not (isLibrary hackage p)
          rv = hackageRevision hackage p
      return $ SusePackageDescription rv isExe

    getCompilerVersion <- addOracle $ \(StackageVersion stackageVersion) ->
      stripSpaces <$> readFile' ("config" </> stackageVersion </> "compiler")

    getFlagAssignment <- addOracle $ \(PackageName pn) ->
      return (flagAssignment pn)

    forM_ (nub (concat packageSets)) $ \p@(PackageIdentifier (PackageName pn) v) -> do
      let pv = display v
          pid = display p
          tarsrc = homeDir </> ".cabal/packages/hackage.haskell.org" </> pn </> pv </> pid <.> "tar.gz"
      tarsrc %> \out -> do
        b <- liftIO $ System.Directory.doesFileExist out
        unless b $
          command_ [] "cabal" ["fetch", "-v0", "--no-dependencies", "--", display p]

    -- TODO: use pattern rules instead of a loop (https://github.com/opensuse-haskell/cabal2obs/issues/2)
    forM_ (zip stackageVersions packageSets) $ \(stackageVersion,stackage) ->
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
          need [tarsrc]
          b <- liftIO $ System.Directory.doesFileExist tar
          unless b $
            bash [ "rm -f " ++ pkgDir ++ "/*.tar.gz", "cp " ++ tarsrc ++ " " ++ out ]

        when (rv > 0) $ do
          want [editedCabalFile]
          editedCabalFile %> \_ -> do
            SusePackageDescription _ _ <- getSusePkgDescription p
            bash ["cd " ++ pkgDir, "rm -f *.cabal", "wget -q " ++ editedCabalFileUrl]

        spec %> \_ -> do
          SusePackageDescription _ _ <- getSusePkgDescription p
          when (rv > 0) $ do
            need [editedCabalFile]
          compiler <- getCompilerVersion (StackageVersion stackageVersion)
          patches <- sort <$> getDirectoryFiles "" [ "patches/common/" ++ pn ++ "/*.patch"
                                                   , "patches/" ++ stackageVersion ++ "/" ++ pn ++ "/*.patch"
                                                   ]
          let clvid = unwords ["version", pv, "revision", show rv]
          need patches
          flags <- getFlagAssignment (PackageName pn)
          bash $ [ "cd " ++ pkgDir
                 , "rm -f *.spec"
                 , "../../../tools/cabal-rpm/dist/build/cabal-rpm/cabal-rpm --strict --compiler=" ++
                   compiler ++ (if forcedExe then " -b " else " ") ++ "--distro=SUSE " ++
                   flags ++ " " ++
                   "spec " ++ pid ++ " >/dev/null"
                 , "spec-cleaner -i " ++ pkgName <.> "spec"
                 , "grep -q -s -F -e '" ++ clvid ++ "' " ++ pkgName <.> "changes" ++
                   "|| osc vc -m 'Update to " ++ clvid ++ " with cabal2obs.'"
                 ] ++
                 [ "patch --no-backup-if-mismatch --force <../../../" ++ pt | pt <- patches ]

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

-- TODO: move into config file (https://github.com/opensuse-haskell/cabal2obs/issues/3)
corePackages :: [String]
corePackages =
  [ "array"
  , "base"
  , "bin-package-db"
  , "binary"
  , "bytestring"
  , "Cabal"
  , "containers"
  , "deepseq"
  , "directory"
  , "filepath"
  , "ghc"
  , "ghc-prim"
  , "haskeline"
  , "hoopl"
  , "hpc"
  , "integer-gmp"
  , "pretty"
  , "process"
  , "template-haskell"
  , "terminfo"
  , "time"
  , "transformers"
  , "unix"
  ]

-- TODO: move into config file (https://github.com/opensuse-haskell/cabal2obs/issues/3)
bannedPackages :: [String]
bannedPackages =
  [ "eventstore"        -- doesn't work on 32 bit: https://github.com/YoEight/eventstore/issues/51
  , "freenect"          -- we have no working freenect
  , "hfsevents"
  , "hopenpgp-tools"    -- depends on broken wl-pprint-terminfo
  , "ide-backend-rts"   -- deprecated
  , "lhs2tex"           -- build is hard to fix
  , "luminance-samples" -- example / tutorial code
  , "reedsolomon"       -- needs an old version of LLVM
  , "seqalign"          -- doesn't work on 32 bit: https://github.com/eurekagenomics/SeqAlign/issues/2
  , "stackage"          -- no dummy packages here
  , "stackage-build-plan" -- deprecated
  , "stackage-cabal"    -- deprecated
--  , "stackage-cli"      -- deprecated (but still dependency of stackage-curator)
  , "stackage-sandbox"  -- deprecated
  , "stackage-setup"    -- deprecated
  , "stackage-types"    -- deprecated
  , "Win32"
  , "Win32-extras"
  , "Win32-notify"
  , "wl-pprint-terminfo" -- https://github.com/opensuse-haskell/configuration/issues/7
  ]

-- TODO: move into config file (https://github.com/opensuse-haskell/cabal2obs/issues/3)
forcedExecutablePackages :: [String]
forcedExecutablePackages =
  [ "Agda"
  , "alex"
  , "apply-refact"
  , "asciidiagram"
  , "BlogLiterately"
  , "BNFC"
  , "bustle"
  , "c2hs"
  , "cab"
  , "cabal-install"
  , "cabal-rpm"
  , "cabal2nix"
  , "codex"
  , "cpphs"
  , "cryptol"
  , "darcs"
  , "derive"
  , "diagrams-haddock"
  , "dixi"
  , "doctest"
  , "doctest-discover"
  , "find-clumpiness"
  , "ghc-imported-from"
  , "ghc-mod"
  , "ghcid"
  , "git-annex"
  , "gtk2hs-buildtools"
  , "hackage-mirror"
  , "hackmanager"
  , "handwriting"
  , "hapistrano"
  , "happy"
  , "HaRe"
  , "haskintex"
  , "HaXml"
  , "hdevtools"
  , "hdocs"
  , "highlighting-kate"
  , "hindent"
  , "hledger"
  , "hledger-web"
  , "hlint"
  , "holy-project"
  , "hoogle"
  , "hpack"
  , "hpc-coveralls"
  , "hscolour"
  , "hsdev"
  , "hspec-discover"
  , "ide-backend"
  , "idris"
  , "keter"
  , "leksah-server"
  , "lhs2tex"
  , "markdown-unlit"
  , "microformats2-parser"
  , "misfortune"
  , "modify-fasta"
  , "msi-kb-backlit"
  , "omnifmt"
  , "osdkeys"
  , "pandoc"
  , "pointfree"
  , "pointful"
  , "postgresql-schema"
  , "purescript"
  , "shake"
  , "ShellCheck"
  , "stack"
  , "stack-run-auto"
  , "stackage-curator"
  , "stackage-curator"
  , "stylish-haskell"
  , "texmath"
  , "werewolf"
  , "wordpass"
  , "xmobar"
  , "xmonad"
  , "yi"
  ]

-- TODO: move into config file (https://github.com/opensuse-haskell/cabal2obs/issues/3)
flagAssignment :: String -> String
flagAssignment "cheapskate" = "-fdingus"
flagAssignment "country-codes" = "-fgenerate"
flagAssignment "cryptonite" = "-f-support_aesni -f-support_rdrand -f-support_blake2_sse"
flagAssignment "csv-conduit" = "-fbench"
flagAssignment "direct-sqlite" = "-fsystemlib"
flagAssignment "Earley" = "-fexamples"
flagAssignment "GLUT" = "-fbuildexamples"
flagAssignment "gtk3" = "-fbuild-demos"
flagAssignment "highlighting-kate" = "-fexecutable -fpcre-light"
flagAssignment "hlibsass" = "-fexternalLibsass"
flagAssignment "hoauth2" = "-ftest"
flagAssignment "hslogger" = "-fbuildtests"
flagAssignment "hslua" = "-fsystem-lua"
flagAssignment "http2" = "-fdevel"
flagAssignment "ipython-kernel" = "-fexamples"
flagAssignment "lambdacube-gl" = "-ftestclient -fexample"
flagAssignment "language-lua2" = "-fexes"
flagAssignment "leveldb-haskell" = "-fexamples"
flagAssignment "nested-routes" = "-fexample"
flagAssignment "network-transport-zeromq" = "-finstall-benchmarks -fdistributed-process-tests"
flagAssignment "nix-paths" = "-fallow-relative-paths"
flagAssignment "pandoc-citeproc" = "-ftest_citeproc"
flagAssignment "rethinkdb" = "-fdev"
flagAssignment "servant-js" = "-fexample"
flagAssignment "SHA" = "-fexe"
flagAssignment "texmath" = "-fnetwork-uri -fexecutable"
flagAssignment "vector-algorithms" = "-fbench"
flagAssignment "wai-middleware-consul" = "-fexample"
flagAssignment "wai-middleware-verbs" = "-fexample"
flagAssignment "waitra" = "-fexamples"
flagAssignment "weigh" = "-fweigh-maps"
flagAssignment "xmobar" = "-fwith_thread -fwith_utf8 -fwith_xft -fwith_xpm -fwith_mpris -fwith_dbus -fwith_iwlib -fwith_inotify"
flagAssignment "yesod-auth-oauth2" = "-fexample -fnetwork-uri"
flagAssignment "yesod-job-queue" = "-fexample"
flagAssignment "yesod-static-angular" = "-fexample"
flagAssignment _ = ""

parseExtraConfig :: Hackage -> String -> [PackageIdentifier]
parseExtraConfig hackage = map f . lines
  where
    f :: String -> PackageIdentifier
    f n = PackageIdentifier (PackageName n) (fst (findMax (getPkg n)))

    getPkg :: String -> Map Version GenericPackageDescription
    getPkg n = fromMaybe (error ("undefined package " ++ show n)) (DB.lookup n hackage)
