{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main ( main ) where

import Cabal2Spec
import Config
import ChangesFile
import Oracle
import Orphans ()
import ParseStackageConfig
import ParseUtils
import Types

import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.SPDX
import Development.Shake
import Development.Shake.FilePath
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Text
import Distribution.Types.ComponentRequestedSpec
import System.Directory
import System.Environment

type instance RuleResult (PackageSetId, PackageName) = FlagAssignment
type instance RuleResult (PackageSetId, BuildName) = PackageIdentifier
type instance RuleResult (PackageSetId, PackageIdentifier) = BuildName

main :: IO ()
main = do
  homeDir <- System.Environment.getEnv "HOME"
  let buildDir = "_build"
      hackageDir = "hackage"

      shopts = shakeOptions
               { shakeFiles = buildDir
               , shakeVerbosity = Quiet
               , shakeProgress = progressDisplay 5 putStrLn
               , shakeChange = ChangeModtimeAndDigest
               , shakeThreads = 0       -- autodetect the number of available cores
               , shakeVersion = "18"    -- version of the build rules, bump to trigger full re-build
               }

  shakeArgs shopts $ do

    -- Register our custom oracles to query the configuration files.
    getCabal <- hackageDB hackageDir
    resolver <- resolveConstraint hackageDir
    packageList <- getPackageList resolver
    forcedExes <- getForcedExes
    compilerId <- getCompiler
    flagOverrides <- getFlagAssignments

    getFlags <- addOracle $ \(psid@(PackageSetId _), n) ->
      fromMaybe mempty . lookup n <$> flagOverrides (GetFlagAssignments psid)

    getBuildName <- addOracle $ \(psid@(PackageSetId _), pkgid@(PackageIdentifier _ _)) -> do
      fexeset <- forcedExes (GetForcedExes psid)
      cabal <- getCabal pkgid
      let forceExe = packageName pkgid `elem` fexeset
          prefix | forceExe || not (hasLibrary cabal)  = ""
                 | otherwise                           = "ghc-"
      return $ BuildName (prefix ++ unPackageName (packageName pkgid))

    pkgidFromPath <- addOracle $ \(PackageSetId psid, BuildName bn) -> do
      pset <- askOracle (GetPackageList (PackageSetId psid)) :: Action [PackageIdentifier]
      id1 <- case find (\(PackageIdentifier n _) -> unPackageName n == bn) pset of
               Nothing -> return Nothing
               Just pkgid -> do BuildName bn' <- getBuildName (PackageSetId psid, pkgid)
                                return (if bn == bn' then Just pkgid else Nothing)
      id2 <- case stripPrefix "ghc-" bn of
               Nothing -> return Nothing
               Just bn' -> case find (\(PackageIdentifier n _) -> unPackageName n == bn') pset of
                             Nothing -> return Nothing
                             Just pkgid -> do BuildName bn'' <- getBuildName (PackageSetId psid, pkgid)
                                              return (if bn == bn'' then Just pkgid else Nothing)
      case (id1,id2) of
        (Nothing, Nothing) -> fail $ "no package called " ++ show bn ++ " configured in " ++ psid ++ " package set"
        (Just pkgid, _)    -> return pkgid
        (_, Just pkgid)    -> return pkgid

    -- Compute all configured builds and register the required targets.
    action $ do
      targets <- forP knownPackageSets $ \psid -> do
        pset <- packageList (GetPackageList psid)
        fexeset <- forcedExes (GetForcedExes psid)
        forP pset $ \pkgid@(PackageIdentifier pn _) -> do
          cabal <- getCabal pkgid
          let isForcedExe = pn `elem` fexeset
              isExe = isForcedExe || not (hasLibrary cabal)
              bn = (if isExe then "" else "ghc-") ++ unPackageName pn
              pkgDir = buildDir </> unPackageSetId psid </> bn
          return [ pkgDir </> bn <.> "spec" ]
      need ((buildDir </> "packages.csv") : concat (concat targets))
      -- get rid of *.orig and *.rej files created by patch(1).
      removeFilesAfter buildDir ["*/*/*.orig", "*/*/*.rej"]

    -- Pattern target to trigger source tarball downloads with "cabal fetch". We
    -- prefer this over direct downloading becauase "cabal" acts as a cache for
    -- us, too.
    homeDir </> ".cabal/packages/hackage.haskell.org/*/*/*.tar.gz" %> \out -> do
      exists <- liftIO $ System.Directory.doesFileExist out
      -- The explicit test for existence is necessary because shake will
      -- re-build existing files if its internal database does not know how the
      -- file was created.
      unless exists $ do
        let pkgid = dropExtension (takeBaseName out)
        command_ [Traced "cabal-fetch"] "cabal" ["fetch", "-v0", "--no-dependencies", "--", pkgid]

    -- Pattern rule that copies the required source tarballs from cabal's
    -- internal cache into our build tree.
    buildDir </> "*/*/*.tar.gz" %> \out -> do
      liftIO $ removeFiles (takeDirectory out) ["*.tar.gz"]
      let pkgid = dropExtension (takeBaseName out)
      PackageIdentifier n v <- parseText "package id" pkgid
      if n == "git-annex"
         then command_ [FileStdout out] "curl"
                       [ "-L", "--silent", "--show-error", "--"
                       , "https://github.com/peti/git-annex/archive/" ++ display v ++".tar.gz"
                       ]
         else copyFile' (homeDir </> ".cabal/packages/hackage.haskell.org" </> unPackageName n </> display v </> pkgid <.> "tar.gz") out

    buildDir </> "*/*/*.changes" %> \out -> do
      let [_,psid',bn',_] = splitDirectories out
          psid = PackageSetId psid'
          bn = BuildName bn'
      pkgid@(PackageIdentifier _ v) <- pkgidFromPath (psid,bn)
      cabal <- getCabal pkgid
      let rev = packageRevision cabal
          versionString = "version " ++ display v
      liftIO $ do changes <- readFile out `mplus` return ""
                  unless (versionString `isInfixOf` changes) $ do
                     newEntry <- mkChangeEntry pkgid rev "psimons@suse.com"
                     writeFile out (newEntry ++ changes)

    -- Pattern rule that generates the package's spec file.
    buildDir </> "*/*/*.spec" %> \out -> do
      need [out -<.> "changes"]
      let [_,psid',bn',_] = splitDirectories out
          psid = PackageSetId psid'
          bn = BuildName bn'
      pkgid@(PackageIdentifier n _) <- pkgidFromPath (psid,bn)
      let isExe = unPackageName n == bn'
          pkgDir = takeDirectory out
      cid <- compilerId (GetCompiler psid)
      fa <- getFlags (psid, n)
      cabal <- getCabal pkgid
      let rev = packageRevision cabal
      if rev > 0
         then copyFile' (cabalFilePath hackageDir pkgid) (pkgDir </> unPackageName n <.> "cabal")
         else liftIO (removeFiles pkgDir ["*.cabal"])
      need [pkgDir </> display pkgid <.> "tar.gz"]
      case finalizePD fa (ComponentRequestedSpec False False) (const True) (Platform X86_64 Linux) (unknownCompilerInfo cid NoAbiTag) [] cabal of
        Left missing -> fail ("finalizePD: " ++ show missing)
        Right (desc,_) -> traced "cabal2spec" (createSpecFile out desc isExe fa)
      Stdout year' <- command [Traced "find-copyright-year"] "sed" ["-r", "-n", "-e", "s/.* [0-9][0-9]:[0-9][0-9]:[0-9][0-9] UTC ([0-9]+) - .*/\\1/p", out -<.> "changes"]
      let year = head (lines year')
      command_ [Cwd "tools/spec-cleaner", Traced "spec-cleaner"] "python3" ["-m", "spec_cleaner", "--copyright-year=" ++ year, "-i", "../.." </> out]
      patches <- getDirectoryFiles "" [ "patches/common/" ++ display n ++ "/*.patch"
                                      , "patches/" ++ psid' ++ "/" ++ display n ++ "/*.patch"
                                      ]
      need patches
      forM_ (sortBy (compare `on` takeFileName) patches) $ \p -> do
        command_ [] "patch" ["--no-backup-if-mismatch", "--force", "--silent", out, p]
        command_ [Cwd "tools/spec-cleaner", Traced "spec-cleaner"] "python3" ["-m", "spec_cleaner", "--copyright-year=" ++ year, "-i", "../.." </> out]
      Stdout buf <- command [Traced "verify-license"] "sed" ["-n", "-e", "s/^License: *//p", out]
      mapM_ verifyLicense (lines buf)

    buildDir </> "packages.csv" %> \out -> do
      let psid = PackageSetId "lts-11"
      pset <- packageList (GetPackageList psid)
      ls <- forP pset $ \pkgid@(PackageIdentifier n v) -> do
        BuildName bn <- getBuildName (psid, pkgid)
        let url = "https://build.opensuse.org/package/show/devel:languages:haskell/" ++ bn
        return $ intercalate "," [ show (display n), show (display v), show url]
      writeFile' out (intercalate "\n" ls)

    buildDir </> "cabal-*.config" %> \out -> do
      alwaysRerun
      let lts = drop 6 (takeBaseName out)
          url = "https://www.stackage.org/" ++ lts ++"/cabal.config"
      command_ [FileStdout out] "curl" ["-L", "-s", url]

    "tools/cabal2obs/Config/*/Stackage.hs" %> \out -> do
      let dirname = takeBaseName (takeDirectory out)
          psid = case dirname of
                   "Nightly"     -> "nightly"
                   'L':'T':'S':x -> "lts-" ++ x
                   _              -> error ("invaid cabal2obs config path " ++ show dirname)
      buf <- readFile' (buildDir </> "cabal-" ++ psid <.> "config")
      deps <- runP stackageConfig buf
      writeFileChanged out (mkStackagePackageSetSourcefile dirname deps)

    phony "update" $ need
      [ "tools/cabal2obs/Config" </> getConfigDirname psid </> "Stackage.hs" | psid <- knownPackageSets ]

    phony "fetch" $ do
      pkgs' <- mapM (packageList . GetPackageList) knownPackageSets
      let pkgs :: [PackageIdentifier]
          pkgs = sort (nub (concat pkgs'))
      need [ homeDir </> ".cabal/packages/hackage.haskell.org" </> display pn </> display v </> display pid <.> "tar.gz"
           | pid@(PackageIdentifier pn v) <- pkgs
           ]

verifyLicense :: Monad m => String -> m ()
verifyLicense "SUSE-Public-Domain" = return ()
verifyLicense lic
  | [l] <- parseExpression lic = when (prettyLicenseExpression l /= lic) $ fail $
                                   unwords [ "license expression"
                                           , lic
                                           , "doesn't match expected"
                                           , prettyLicenseExpression l
                                           ]
  | otherwise                  = fail (unwords ["invalid license expression", show lic])

mkStackagePackageSetSourcefile :: String -> [Dependency] -> String
mkStackagePackageSetSourcefile vers deps = unlines
  [ "{-# LANGUAGE OverloadedStrings #-}"
  , "{-# OPTIONS_GHC -fno-warn-deprecations #-}"
  , ""
  , "module Config." ++ vers ++ ".Stackage where"
  , ""
  , "import Orphans ( )"
  , "import Distribution.Package"
  , ""
  , "stackage :: [Dependency]"
  , "stackage ="
  , "  [ " ++ intercalate "\n  , " (map (show . display) deps)
  , "  ]"
  ]
