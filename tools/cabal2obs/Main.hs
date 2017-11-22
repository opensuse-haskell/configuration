{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main ( main ) where

import Cabal2Spec
import Config ( knownPackageSets )
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
import Data.Time
import Development.Shake
import Development.Shake.FilePath
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Types.ComponentRequestedSpec
import Distribution.System
import Distribution.Text
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
               , shakeVersion = "8"     -- version of the build rules, bump to trigger full re-build
               }

  shakeArgs shopts $ do

    -- Register our custom oracles to query the configuration files.
    getCabal <- hackageDB hackageDir
    resolver <- resolveConstraint hackageDir
    packageList <- getPackageList resolver
    forcedExes <- getForcedExes
    compilerId <- getCompiler
    flagOverrides <- getFlagAssignments

    getFlags <- addOracle $ \(psid@(PackageSetId _), n) -> do
      fas <- flagOverrides (GetFlagAssignments psid)
      return $ fromMaybe [] (lookup n fas)

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

    -- Pattern rule that generates the package's spec file.
    buildDir </> "*/*/*.spec" %> \out -> do
      let [_,psid',bn',_] = splitDirectories out
          psid = PackageSetId psid'
          bn = BuildName bn'
      pkgid@(PackageIdentifier n v) <- pkgidFromPath (psid,bn)
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
        Left missing -> fail ("finalizePackageDescription: " ++ show missing)
        Right (desc,_) -> withTempDir $ \tmpDir -> do
                            command_ [] "tar" ["-C", tmpDir, "-x", "-f", pkgDir </> display pkgid <.> "tar.gz"]
                            copyFile' (cabalFilePath hackageDir pkgid) (tmpDir </> display pkgid </> display n <.> "cabal")
                            traced "cabal2spec" $ do
                              createSpecFile out (tmpDir </> display pkgid </> display pkgid <.> "cabal") desc isExe fa
      command_ [Cwd "tools/spec-cleaner", Traced "spec-cleaner"] "python3" ["-m", "spec_cleaner", "-i", "../.." </> out]

      patches <- getDirectoryFiles "" [ "patches/common/" ++ display n ++ "/*.patch"
                                      , "patches/" ++ psid' ++ "/" ++ display n ++ "/*.patch"
                                      ]
      need patches
      forM_ (sortBy (compare `on` takeFileName) patches) $ \p -> do
        command_ [] "patch" ["--no-backup-if-mismatch", "--force", "--silent", out, p]
        command_ [Cwd "tools/spec-cleaner", Traced "spec-cleaner"] "python3" ["-m", "spec_cleaner", "-i", "../.." </> out]
      Stdout buf <- command [Traced "verify-license"] "sed" ["-n", "-e", "s/^License: *//p", out]
      mapM_ verifyLicense (lines buf)
      let versionString = unwords $ ["version", display v] ++
                                    if rev==0  then [] else ["revision", show rev]
          changesFile = out -<.> "changes"
      liftIO $ do
        changes <- do changesFileExists <- System.Directory.doesFileExist changesFile
                      if changesFileExists then readFile changesFile else return ""
        unless (versionString `isInfixOf` changes) $ do
           newEntry <- mkChangeEntry versionString "psimons@suse.com"
           writeFile changesFile (newEntry ++ changes)

    buildDir </> "packages.csv" %> \out -> do
      let psid = PackageSetId "lts-8"
      pset <- packageList (GetPackageList psid)
      ls <- forP pset $ \pkgid@(PackageIdentifier n v) -> do
        BuildName bn <- getBuildName (psid, pkgid)
        let url = "https://build.opensuse.org/package/show/devel:languages:haskell/" ++ bn
        return $ intercalate "," [ show (display n), show (display v), show url]
      writeFile' out (intercalate "\n" ls)

    buildDir </> "cabal-lts-*.config" %> \out -> do
      alwaysRerun
      let lts = drop 10 (takeBaseName out)
          url = "https://www.stackage.org/lts-" ++ lts ++"/cabal.config"
      command_ [FileStdout out] "curl" ["-L", "-s", url]

    "tools/cabal2obs/Config/LTS*/Stackage.hs" %> \out -> do
      let lts = drop 3 (takeBaseName (takeDirectory out))
      buf <- readFile' (buildDir </> "cabal-lts-" ++ lts <.> "config")
      deps <- runP stackageConfig buf
      writeFileChanged out (mkStackagePackageSetSourcefile lts deps)

    phony "update" $ need
      [ "tools/cabal2obs/Config/LTS"++drop 4 psid++"/Stackage.hs" | PackageSetId psid <- knownPackageSets ]

mkChangeEntry :: String -> String -> IO String
mkChangeEntry version email = do
  ts <- formatTime defaultTimeLocale "%a %b %_d %H:%M:%S %Z %Y" <$> getCurrentTime
  return $ unlines
    [ "-------------------------------------------------------------------"
    , unwords [ ts, "-", email ]
    , ""
    , "- Update to " ++ version ++ "."
    , "  A more detailed change log is not available."
    , ""
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
  , "module Config.LTS" ++ vers ++ ".Stackage where"
  , ""
  , "import Orphans ( )"
  , "import Distribution.Package"
  , ""
  , "stackage :: [Dependency]"
  , "stackage ="
  , "  [ " ++ intercalate "\n  , " (map (show . display) deps)
  , "  ]"
  ]
