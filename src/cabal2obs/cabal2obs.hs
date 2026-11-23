{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main ( main ) where

import Prelude hiding ( fail )

import Cabal2Spec
import Oracle
import Types
import UpdateChangesFile
import UpdatePackageSet ( updatePackageSetSpecInPlace )
import MyCabal

import Control.Monad.Extra
import qualified Data.ByteString as BSS
import Data.Function
import Data.List as List ( intercalate, stripPrefix, sortBy )
import qualified Data.Map.Strict as Map
import Data.Maybe ( fromMaybe )
import qualified Data.Set as Set
import Development.Shake as Shake
import Development.Shake.FilePath
import System.Console.GetOpt ( ArgDescr(..), OptDescr(..) )
import System.Directory
import System.Environment ( lookupEnv )

type instance RuleResult BuildName = PackageIdentifier
type instance RuleResult PackageIdentifier = BuildName

main :: IO ()
main = do
  python3 <- fromMaybe "python3" <$> lookupEnv "PYTHON3"
  cabalDir <- getAppUserDataDirectory "cabal"
  let buildDir = "_build"
      shopts = shakeOptions
               { shakeFiles = buildDir
               , shakeVerbosity = Warn
               , shakeProgress = progressDisplay 5 putStrLn
               , shakeChange = ChangeModtimeAndDigest
               , shakeThreads = 0       -- autodetect the number of available cores
               , shakeVersion = "50"    -- version of the build rules, bump to trigger full re-build
               }

  shakeArgsWith shopts [configOption] $ \opts targets -> pure $ Just $ withTargets targets $ do

    -- Read the specification of our package set. The name of the package set,
    -- e.g. "ghc-9.14.x", names targets and directories, so it is needed while
    -- the rules are defined. The specification itself is accessed only through
    -- an Oracle so that we know what to re-build when the configuration
    -- changes. The package set name doesn't need that mechanism, because it's
    -- baked into every single path, so if the package set name changes, we
    -- re-build everything anyway. The maintainer is read here, too, so that
    -- the .changes rules don't depend on the spec oracle: a different
    -- maintainer matters only for new .changes entries, which pick up the
    -- current value anyway.
    --
    -- TODO: It should be possible to register multiple config files by giving
    -- --config several times. The build system should then configure itself
    -- to build all of those package sets concurrently. For now, the last
    -- --config wins.
    let configFile = last (defaultConfigFile : [ f | ConfigFile f <- opts ])
    (pkgSetName, maintainerAddress, getPackageSetSpec) <- do spec <- readPackageSetSpec configFile
                                                             oracle <- addPackageSetSpecOracle spec
                                                             pure (packageSetName spec, getMaintainer spec, oracle)
    getPackageSet        <- addPackageSetOracle getPackageSetSpec
    getAvailablePackages <- addAvailablePackagesOracle getPackageSetSpec
    getFlags             <- addFlagAssignmentOracle getPackageSetSpec
    isForcedExecutable   <- addForcedExecutableOracle getPackageSetSpec
    getCompiler          <- addCompilerOracle getPackageSetSpec

    -- Look up the Cabal file for a given PackageIdentifier from Hackage.
    getHackage <- addHackageCache
    getCabal <- addCabalFileCache getHackage
    getPreferredVersions <- addPreferredVersionsOracle getHackage

    -- Custom oracle to figure out the rpm package name used for a given build.
    getBuildName <- addOracle $ \pkgid@(PackageIdentifier pn _) -> do
      (_, cbuf) <- getCabal pkgid
      cabal <- parseCabalFile pkgid cbuf
      forceExe <- isForcedExecutable pn
      let prefix | forceExe || not (hasLibrary cabal)  = ""
                 | otherwise                           = "ghc-"
      pure $ BuildName (prefix ++ unPackageName pn)

    -- Map a build directory path back to a Cabal package identifier. This is
    -- the inverse of 'getBuildName'.
    pkgidFromPath <- addOracle $ \(BuildName bn) -> do
      pkgSet <- getPackageSet
      id1 <- case Map.lookup (mkPackageName bn) pkgSet of
               Nothing -> pure Nothing
               Just pv -> do let pkgid = PackageIdentifier (mkPackageName bn) pv
                             BuildName bn' <- getBuildName pkgid
                             pure (if bn == bn' then Just pkgid else Nothing)
      id2 <- case stripPrefix "ghc-" bn of
               Nothing  -> pure Nothing
               Just bn' -> case Map.lookup (mkPackageName bn') pkgSet of
                             Nothing -> pure Nothing
                             Just pv -> do let pkgid = PackageIdentifier (mkPackageName bn') pv
                                           BuildName bn'' <- getBuildName pkgid
                                           pure (if bn == bn'' then Just pkgid else Nothing)
      case (id1,id2) of
        (Nothing, Nothing) -> fail $ "package set " ++ show pkgSetName ++ " contains no build called " ++ show bn
        (Just pkgid, _)    -> pure pkgid
        (_, Just pkgid)    -> pure pkgid


    -- Removed left-overs from failed attempts to apply patches.
    action $ removeFilesAfter buildDir ["*/*/*.orig", "*/*/*.rej"]

    -- By default we build the (phony) all target.
    want ["all"]

    -- Depend on all active package set targets.
    phony "all" $
      need [pkgSetName]

    -- The (phony) package set target depends on the (real) spec files.
    phony pkgSetName $ do
      pkgSet <- getPackageSet
      specFiles <- forM (Map.toList pkgSet) $ \(pn,pv) -> do
        BuildName bn <- getBuildName (PackageIdentifier pn pv)
        pure (buildDir </> pkgSetName </> bn </> bn <.> "spec")
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
                         , "https://github.com/opensuse-haskell/git-annex/archive/" ++ display v ++".tar.gz"
                         ]
           else copyFile' (cabalDir </> "packages/hackage.haskell.org" </> unPackageName n </> display v </> pkgid <.> "tar.gz") out

    buildDir </> "*/*/*.changes" %> \out -> do
      bn <- extractBuildName pkgSetName out
      pkgid@(PackageIdentifier pn v) <- pkgidFromPath bn
      (ctime, cbuf) <- getCabal pkgid
      cabal <- parseCabalFile pkgid cbuf
      let rev = packageRevision cabal
      traced "update-changes-file" $ updateChangesFile (Just ctime) out pn (v, rev) maintainerAddress

    -- Pattern rule that generates the package's spec file.
    buildDir </> "*/*/*.spec" %> \out -> do
      bn <- extractBuildName pkgSetName out
      pkgid@(PackageIdentifier n v) <- pkgidFromPath bn
      let isExe = unPackageName n == unBuildName bn
          pkgDir = takeDirectory out
      -- Warn if Hackage has deprecated the version we generate a build for.
      shakeOpts <- getShakeOptions
      when (shakeVerbosity shakeOpts >= Warn) $ do
        Dependency _ preferred _ <- getPreferredVersions n
        unless (v `withinRange` preferred) $ do
          liftIO $ shakeOutput shakeOpts Warn $
            "warning: " ++ display pkgid ++ " is a deprecated version; Hackage prefers " ++ display n ++ " " ++ display preferred
      fa <- getFlags n
      (_, cabalFile) <- getCabal pkgid
      cabal <- parseCabalFile pkgid cabalFile
      let rev = packageRevision cabal
      if rev > 0
         then liftIO (BSS.writeFile (pkgDir </> unPackageName n <.> "cabal") cabalFile)
         else liftIO (removeFiles pkgDir ["*.cabal"])
      need [pkgDir </> display pkgid <.> "tar.gz", out -<.> "changes"]
      available <- getAvailablePackages
      cid <- getCompiler
      let lookupPackageSet d = depPkgName d `Set.member` available || depPkgName d `Map.member` localLibraries cabal
      case finalizePD fa (ComponentRequestedSpec False False) lookupPackageSet (Platform X86_64 Linux) (unknownCompilerInfo cid NoAbiTag) [] cabal of
        Left missing -> fail ("missing dependencies in package set: " ++ intercalate ", " (prettyShow <$> missing))
        Right (desc,_) -> traced "cabal2spec" (createSpecFile out desc isExe False fa Nothing)
      Stdout year' <- command [Traced "find-copyright-year"] "sed" ["-r", "-n", "-e", "s/.* [0-9][0-9]:[0-9][0-9]:[0-9][0-9] UTC ([0-9]+) - .*/\\1/p", out -<.> "changes"]
      year <- case lines year' of
                []  -> fail ("impossible year value " ++ show year' ++ " returned")
                y:_ -> pure y
      command_ [Cwd "src/spec-cleaner", Traced "spec-cleaner"] python3 ["-m", "spec_cleaner", "--suse-copyright", "--copyright-year=" ++ year, "-i", "../.." </> out]
      patches <- getDirectoryFiles "" [ "patches/common/" ++ display n ++ "/*.patch"
                                      , "patches/" ++ pkgSetName ++ "/" ++ display n ++ "/*.patch"
                                      ]
      need patches
      forM_ (sortBy (compare `on` takeFileName) patches) $ \p -> do
        command_ [] "patch" ["--no-backup-if-mismatch", "--force", "--silent", out, p]
        command_ [Cwd "src/spec-cleaner", Traced "spec-cleaner"] python3 ["-m", "spec_cleaner", "--suse-copyright", "--copyright-year=" ++ year, "-i", "../.." </> out]
      Stdout buf <- command [Traced "verify-license"] "sed" ["-n", "-e", "s/^License: *//p", out]
      mapM_ verifyLicense (lines buf)

    -- Resolve the package set anew with cabal-install from the current
    -- Hackage index, raise the bounds of the target packages to the resolved
    -- versions, and write both into the package-set and target-packages
    -- sections of the configuration file. The rest of the file stays as it
    -- is. This is expensive, so it runs only when "update" is requested.
    phony "update" $ do
      getPackageSetSpec >>= updatePackageSetSpecInPlace configFile

    phony "fetch" $ do
      pkgSet <- getPackageSet
      let tarball (pn, pv) = cabalDir </> "packages/hackage.haskell.org" </> display pn </> display pv </> display (PackageIdentifier pn pv) <.> "tar.gz"
      need (tarball <$> Map.toList pkgSet)

-- | Command-line options of cabal2obs in addition to Shake's own options.

newtype Flag = ConfigFile FilePath

configOption :: OptDescr (Either String Flag)
configOption = Option "" ["config"] (ReqArg (Right . ConfigFile) "PATH")
  ("read the package set specification from PATH (default: " <> defaultConfigFile <> ")")

-- | The package set specification we read unless --config says otherwise.

defaultConfigFile :: FilePath
defaultConfigFile = "conf/default-package-set.yaml"

-- | Build the given targets, or the default targets if there are none. This
-- is what 'shakeArgs' does with the targets given on the command line.

withTargets :: [String] -> Rules () -> Rules ()
withTargets [] rules = rules
withTargets ts rules = want ts >> withoutActions rules

verifyLicense :: MonadFail m => String -> m ()
verifyLicense "SUSE-Public-Domain" = pure ()
verifyLicense lic = case eitherParsec lic of
  Left msg -> fail ("invalid license expression " ++  lic ++ "\n" ++ msg)
  Right l -> let lic' = prettyShow (l :: License)
             in unless (lic == lic') $
               fail ("license " ++ lic ++ " doesn't match expected " ++ lic')

extractBuildName :: MonadFail m => FilePath -> FilePath -> m BuildName
extractBuildName pkgSetName p
  | [_,psn,bn,_] <- splitDirectories p
  , psn == pkgSetName                   = pure (BuildName bn)
  | otherwise                           = fail ("path does not refer to built *.spec or *.changes file: " ++ show p)

localLibraries :: GenericPackageDescription -> PackageSet
localLibraries gpd = Map.fromList (libs1 ++ libs2)
  where
    libs1 = [ (mkPackageName (unUnqualComponentName lname), version0) | lib <- subLibraries (packageDescription gpd), LSubLibName lname <- [libName lib] ]
    libs2 = [ (mkPackageName (unUnqualComponentName lname), version0) | (lname, _) <- condSubLibraries gpd ]
