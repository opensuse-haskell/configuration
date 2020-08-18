{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main ( main ) where

import Prelude hiding ( fail )

import Cabal2Spec
import Config
import Oracle
import Orphans ()
import ParseStackageConfig
import ParseUtils
import Types
import UpdateChangesFile

import Control.Monad.Extra
import Data.Function
import Data.List as List ( intercalate, stripPrefix, sortBy )
import Data.Set as Set ( Set )
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BSS
import Development.Shake as Shake
import Development.Shake.FilePath
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Parsec
import Distribution.Pretty
import Distribution.SPDX
import Distribution.System
import Distribution.Text
import Distribution.Types.ComponentRequestedSpec
import System.Directory

type instance RuleResult (PackageSetId, PackageName) = FlagAssignment
type instance RuleResult (PackageSetId, BuildName) = PackageIdentifier
type instance RuleResult (PackageSetId, PackageIdentifier) = BuildName

main :: IO ()
main = do
  cabalDir <- getAppUserDataDirectory "cabal"
  let buildDir = "_build"
      shopts = shakeOptions
               { shakeFiles = buildDir
               , shakeVerbosity = Quiet
               , shakeProgress = progressDisplay 5 putStrLn
               , shakeChange = ChangeModtimeAndDigest
               , shakeThreads = 0       -- autodetect the number of available cores
               , shakeVersion = "36"    -- version of the build rules, bump to trigger full re-build
               }

  shakeArgs shopts $ do

    -- Register our custom oracles to query the configuration files.
    hackage <- addHackageCache
    _ <- addConstraintResolverOracle hackage
    getCabal <- addCabalFileCache hackage

    getPackageSet <- addConfigOracle

    -- Custom oracle to figure out the rpm package name used for a given build.
    getBuildName <- addOracle $ \(psid@(PackageSetId _), pkgid@(PackageIdentifier pn _)) -> do
      cabal <- getCabal pkgid >>= parseCabalFile pkgid
      pset <- getPackageSet psid
      let forceExe = pn `elem` forcedExectables pset
          prefix | forceExe || not (hasLibrary cabal)  = ""
                 | otherwise                           = "ghc-"
      return $ BuildName (prefix ++ unPackageName pn)

    -- Map a build directory path back to a Cabal package identifirer. This is
    -- the inverse of 'getBuildName'.
    pkgidFromPath <- addOracle $ \(psid, BuildName bn) -> do
      pset <- getPackageSet psid
      let pkgs = packageSet pset
      id1 <- case Map.lookup (mkPackageName bn) pkgs of
               Nothing -> return Nothing
               Just pv -> do let pkgid = PackageIdentifier (mkPackageName bn) pv
                             BuildName bn' <- getBuildName (psid, pkgid)
                             return (if bn == bn' then Just pkgid else Nothing)
      id2 <- case stripPrefix "ghc-" bn of
               Nothing  -> return Nothing
               Just bn' -> case Map.lookup (mkPackageName bn') pkgs of
                             Nothing -> return Nothing
                             Just pv -> do let pkgid = PackageIdentifier (mkPackageName bn') pv
                                           BuildName bn'' <- getBuildName (psid, pkgid)
                                           return (if bn == bn'' then Just pkgid else Nothing)
      case (id1,id2) of
        (Nothing, Nothing) -> fail $ "package set " ++ show (unPackageSetId psid) ++ " contains no build called " ++ show bn
        (Just pkgid, _)    -> return pkgid
        (_, Just pkgid)    -> return pkgid


    -- Removed left-overs from failed attempts to apply patches.
    action $ removeFilesAfter buildDir ["*/*/*.orig", "*/*/*.rej"]

    -- By default we build the (phony) all target.
    want ["all"]

    -- Depend on all active package set targets.
    phony "all" $
      need ["ghc-8.10.x"]

    -- Every (phony) package set target depends on the (real) spec file.
    forM_ (Set.toList knownPackageSets) $ \psid ->
      phony (unPackageSetId psid) $ do
        pset <- getPackageSet psid
        specFiles <- forM (Map.toList (packageSet pset)) $ \(pn,pv) -> do
          BuildName bn <- getBuildName (psid, PackageIdentifier pn pv)
          return (buildDir </> unPackageSetId psid </> bn </> bn <.> "spec")
        need specFiles

    -- Pattern target to trigger source tarball downloads with "cabal fetch". We
    -- prefer this over direct downloading becauase "cabal" acts as a cache for
    -- us, too.
    cabalDir </> "packages/hackage.haskell.org/*/*/*.tar.gz" %> \out ->
      -- The explicit test for existence is necessary because shake will
      -- re-build existing files if its internal database does not know how the
      -- file was created.
      unlessM (liftIO (System.Directory.doesFileExist out)) $ do
        let pkgid = dropExtension (takeBaseName out)
        command_ [Traced "cabal-fetch"] "cabal" ["fetch", "-v0", "--no-dependencies", "--", pkgid]

    -- Pattern rule that copies the required source tarballs from cabal's
    -- internal cache into our build tree.
    buildDir </> "*/*/*.tar.gz" %> \out ->
      unlessM (liftIO (System.Directory.doesFileExist out)) $ do
        liftIO $ removeFiles (takeDirectory out) ["*.tar.gz"]
        let pkgid = dropExtension (takeBaseName out)
        PackageIdentifier n v <- parseText "PackageIdentifier" pkgid
        if n == "git-annex"
           then command_ [FileStdout out] "curl"
                         [ "-L", "--silent", "--show-error", "--"
                         , "https://github.com/peti/git-annex/archive/" ++ display v ++".tar.gz"
                         ]
           else copyFile' (cabalDir </> "packages/hackage.haskell.org" </> unPackageName n </> display v </> pkgid <.> "tar.gz") out

    buildDir </> "*/*/*.changes" %> \out -> do
      (psid, bn) <- extractPackageSetIdAndBuildName out
      PackageIdentifier pn v <- pkgidFromPath (psid,bn)         -- TODO: evil hard-coded constant
      traced "update-changes-file" $ updateChangesFile Nothing out pn v "psimons@suse.com"

    -- Pattern rule that generates the package's spec file.
    buildDir </> "*/*/*.spec" %> \out -> do
      (psid, bn) <- extractPackageSetIdAndBuildName out
      pset <- getPackageSet psid
      pkgid@(PackageIdentifier n _) <- pkgidFromPath (psid,bn)
      let isExe = unPackageName n == unBuildName bn
          pkgDir = takeDirectory out
          cid = compiler pset
          fa = Map.findWithDefault mempty n (flagAssignments pset)
      cabalFile <- getCabal pkgid
      cabal <- parseCabalFile pkgid cabalFile
      let rev = packageRevision cabal
      if rev > 0
         then liftIO (BSS.writeFile (pkgDir </> unPackageName n <.> "cabal") cabalFile)
         else liftIO (removeFiles pkgDir ["*.cabal"])
      need [pkgDir </> display pkgid <.> "tar.gz", out -<.> "changes"]
      case finalizePD fa (ComponentRequestedSpec False False) (const True) (Platform X86_64 Linux) (unknownCompilerInfo cid NoAbiTag) [] cabal of
        Left missing -> fail ("finalizePD: " ++ show missing)
        Right (desc,_) -> traced "cabal2spec" (createSpecFile out desc isExe False fa Nothing)
      Stdout year' <- command [Traced "find-copyright-year"] "sed" ["-r", "-n", "-e", "s/.* [0-9][0-9]:[0-9][0-9]:[0-9][0-9] UTC ([0-9]+) - .*/\\1/p", out -<.> "changes"]
      let year = head (lines year')
      command_ [Cwd "tools/spec-cleaner", Traced "spec-cleaner"] "python3" ["-m", "spec_cleaner", "--copyright-year=" ++ year, "-i", "../.." </> out]
      patches <- getDirectoryFiles "" [ "patches/common/" ++ display n ++ "/*.patch"
                                      , "patches/" ++ unPackageSetId psid ++ "/" ++ display n ++ "/*.patch"
                                      ]
      need patches
      forM_ (sortBy (compare `on` takeFileName) patches) $ \p -> do
        command_ [] "patch" ["--no-backup-if-mismatch", "--force", "--silent", out, p]
        command_ [Cwd "tools/spec-cleaner", Traced "spec-cleaner"] "python3" ["-m", "spec_cleaner", "--copyright-year=" ++ year, "-i", "../.." </> out]
      Stdout buf <- command [Traced "verify-license"] "sed" ["-n", "-e", "s/^License: *//p", out]
      mapM_ verifyLicense (lines buf)

    buildDir </> "packages.csv" %> \out -> do
      let psid = PackageSetId "ghc-8.6.x"
      pkgs <- packageSet <$> getPackageSet psid
      ls <- forM (Map.toList pkgs) $ \(pn, v) -> do
        BuildName bn <- getBuildName (psid, PackageIdentifier pn v)
        let url = "https://build.opensuse.org/package/show/devel:languages:haskell/" ++ bn
        return $ intercalate "," [ show (display pn), show (display v), show url]
      writeFile' out (intercalate "\n" ls) -- cannot use 'unlines' because the file mustn't end with a new line

    buildDir </> "cabal-*.config" %> \out -> do
      alwaysRerun
      let lts = drop 6 (takeBaseName out)
          url = "https://www.stackage.org/" ++ lts ++"/cabal.config"
      command_ [FileStdout out] "curl" ["-L", "-s", url]

    "src/Config/*/Stackage.hs" %> \out -> do
      let dirname = takeBaseName (takeDirectory out)
          psid = case dirname of
                   "Nightly"     -> "nightly"
                   'L':'T':'S':x -> "lts-" ++ x
                   _              -> error ("invalid cabal2obs config path " ++ show dirname)
      buf <- readFile' (buildDir </> "cabal-" ++ psid <.> "config")
      deps <- parse stackageConfig buf
      writeFileChanged out (mkStackagePackageSetSourcefile dirname deps)

    -- phony "update" $ need
    --   [ "tools/cabal2obs/Config" </> getConfigDirname psid </> "Stackage.hs" | psid <- knownPackageSets ]

    phony "fetch" $ do
      psets <- mapM (fmap packageSet . askOracle) (Set.toList knownPackageSets)
      let pids :: [Set PackageIdentifier]
          pids = Map.foldrWithKey (\pn pv s -> Set.insert (PackageIdentifier pn pv) s) mempty <$> psets
          tarball pid@(PackageIdentifier pn pv) = cabalDir </> "packages/hackage.haskell.org" </> display pn </> display pv </> display pid <.> "tar.gz"
      need (Set.toList (Set.map tarball (Set.unions pids)))

verifyLicense :: MonadFail m => String -> m ()
verifyLicense "SUSE-Public-Domain" = return ()
verifyLicense lic = case eitherParsec lic of
  Left msg -> fail ("invalid license expression " ++  lic ++ "\n" ++ msg)
  Right l -> let lic' = prettyShow (l :: License)
             in unless (lic == lic') $
               fail ("license " ++ lic ++ " doesn't match expected " ++ lic')

mkStackagePackageSetSourcefile :: String -> [Dependency] -> String
mkStackagePackageSetSourcefile vers deps = unlines
  [ "{-# LANGUAGE OverloadedStrings #-}"
  , "{-# LANGUAGE OverloadedLists #-}"
  , ""
  , "module Config." ++ vers ++ ".Stackage where"
  , ""
  , "import Types"
  , ""
  , "stackage :: ConstraintSet"
  , "stackage ="
  , "  [ " ++ intercalate "\n  , " (map (show . display) deps)
  , "  ]"
  ]

extractPackageSetIdAndBuildName :: MonadFail m => FilePath -> m (PackageSetId, BuildName)
extractPackageSetIdAndBuildName p
  | [_,psid,bn,_] <- splitDirectories p = return (PackageSetId psid, BuildName bn)
  | otherwise                           = fail ("path does not refer to built *.spec or *.changes file: " ++ show p)
