{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main ( main ) where

import Cabal2Spec
import Config
import ChangesFile
import Oracle
import Orphans ()
-- import ParseStackageConfig
import ParseUtils
import Types

import Control.Monad.Extra
import Data.Function
import Data.List as List
-- import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
-- import Data.Maybe
import Development.Shake as Shake
import Development.Shake.FilePath
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.SPDX
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
               , shakeVersion = "25"    -- version of the build rules, bump to trigger full re-build
               }

  shakeArgs shopts $ do

    -- Register our custom oracles to query the configuration files.
    getCabal <- hackageDB hackageDir

    -- Custom oracle to figure out the rpm package name used for a given build.
    getBuildName <- addOracle $ \(psid@(PackageSetId _), pkgid@(PackageIdentifier pn _)) -> do
      cabal <- getCabal pkgid
      let forceExe = pn `elem` forcedExectables (getPackageSet psid)
          prefix | forceExe || not (hasLibrary cabal)  = ""
                 | otherwise                           = "ghc-"
      return $ BuildName (prefix ++ unPackageName pn)

    -- Map a build directory path back to a Cabal package identifirer. This is
    -- the inverse of 'getBuildName'.
    pkgidFromPath <- addOracle $ \(psid, BuildName bn) -> do
      let pset = getPackageSet psid
          pkgs = packageSet pset
      id1 <- case Map.lookup (mkPackageName bn) pkgs of
               Nothing -> return Nothing
               Just pv -> do let pkgid = PackageIdentifier (mkPackageName bn) pv
                             BuildName bn' <- getBuildName (psid, pkgid)
                             return (if bn == bn' then Just pkgid else Nothing)
      id2 <- case stripPrefix "ghc-" bn of
               Nothing  -> return Nothing
               Just bn' -> do case Map.lookup (mkPackageName bn') pkgs of
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

    -- Depend on all (phony) package set targets.
    phony "all" $ do
      need (map unPackageSetId (Map.keys packageSets))

    -- Every (phony) package set target depends on the (real) spec file.
    forM_ (Map.toList packageSets) $ \(psid,pset) -> do
      phony (unPackageSetId psid) $ do
        specFiles <- forM (Map.toList (packageSet pset)) $ \(pn,pv) -> do
          BuildName bn <- getBuildName (psid, PackageIdentifier pn pv)
          return (buildDir </> unPackageSetId psid </> bn </> bn <.> "spec")
        need specFiles

    -- Generate the latest changelog entry template for every TW package.
    phony "changelogs" $ do
      let psid = "ghc-8.4.x"
          pset = getPackageSet psid    -- TODO: hard-coded magic constant
      changesFiles <- forM (Map.toList (packageSet pset)) $ \(pn,pv) -> do
         BuildName bn <- getBuildName (psid, PackageIdentifier pn pv)
         return (buildDir </> unPackageSetId psid </> bn </> bn <.> "changes")
      need changesFiles

    -- Pattern target to trigger source tarball downloads with "cabal fetch". We
    -- prefer this over direct downloading becauase "cabal" acts as a cache for
    -- us, too.
    homeDir </> ".cabal/packages/hackage.haskell.org/*/*/*.tar.gz" %> \out -> do
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
           else copyFile' (homeDir </> ".cabal/packages/hackage.haskell.org" </> unPackageName n </> display v </> pkgid <.> "tar.gz") out

    buildDir </> "*/*/*.changes" %> \out -> do
      let [_,psid',bn',_] = splitDirectories out
          psid = PackageSetId psid'
          bn = BuildName bn'
      pkgid@(PackageIdentifier _ v) <- pkgidFromPath (psid,bn)
      cabal <- getCabal pkgid
      let rev = packageRevision cabal
          versionString = "version " ++ display v
      changes <- liftIO (readFile out `mplus` return "")
      unless (versionString `isInfixOf` changes) $
         traced "update-changelog" $ do
           newEntry <- mkChangeEntry pkgid rev "psimons@suse.com" -- TODO: hard-coded magic constant
           writeFile out (newEntry ++ changes)

    -- Pattern rule that generates the package's spec file.
    buildDir </> "*/*/*.spec" %> \out -> do
      let [_,psid',bn',_] = splitDirectories out
          psid = PackageSetId psid'
          bn = BuildName bn'
          pset = getPackageSet psid
      pkgid@(PackageIdentifier n _) <- pkgidFromPath (psid,bn)
      let isExe = unPackageName n == bn'
          pkgDir = takeDirectory out
          cid = compiler pset
          fa = Map.findWithDefault mempty n (flagAssignments pset)
      cabal <- getCabal pkgid
      let rev = packageRevision cabal
      if rev > 0
         then copyFile' (cabalFilePath hackageDir pkgid) (pkgDir </> unPackageName n <.> "cabal")
         else liftIO (removeFiles pkgDir ["*.cabal"])
      need [pkgDir </> display pkgid <.> "tar.gz"]
      case finalizePD fa (ComponentRequestedSpec False False) (const True) (Platform X86_64 Linux) (unknownCompilerInfo cid NoAbiTag) [] cabal of
        Left missing -> fail ("finalizePD: " ++ show missing)
        Right (desc,_) -> traced "cabal2spec" (createSpecFile out desc isExe fa)
      -- TODO: There is a subtle problem here. The change log files affect the
      -- spec file generate because they determine the copyright year when they
      -- exist. So, technically, these are dependencies of this rule. We don't
      -- want them to be, however, because we want change logs to be generated
      -- only when they are explicitly requested by the user.
      yearFlag <- do haveChanges <- Shake.doesFileExist (out -<.> "changes")
                     if not haveChanges then return [] else do
                        Stdout year' <- command [Traced "find-copyright-year"] "sed" ["-r", "-n", "-e", "s/.* [0-9][0-9]:[0-9][0-9]:[0-9][0-9] UTC ([0-9]+) - .*/\\1/p", out -<.> "changes"]
                        let year = head (lines year')
                        return ["--copyright-year=" ++ year]
      command_ [Cwd "tools/spec-cleaner", Traced "spec-cleaner"] "python3" $ ["-m", "spec_cleaner"] ++ yearFlag ++ ["-i", "../.." </> out]
      patches <- getDirectoryFiles "" [ "patches/common/" ++ display n ++ "/*.patch"
                                      , "patches/" ++ psid' ++ "/" ++ display n ++ "/*.patch"
                                      ]
      need patches
      forM_ (sortBy (compare `on` takeFileName) patches) $ \p -> do
        command_ [] "patch" ["--no-backup-if-mismatch", "--force", "--silent", out, p]
        command_ [Cwd "tools/spec-cleaner", Traced "spec-cleaner"] "python3" $ ["-m", "spec_cleaner"] ++ yearFlag ++ ["-i", "../.." </> out]
      Stdout buf <- command [Traced "verify-license"] "sed" ["-n", "-e", "s/^License: *//p", out]
      mapM_ verifyLicense (lines buf)

    -- buildDir </> "packages.csv" %> \out -> do
    --   let psid = PackageSetId "lts-11"
    --   pset <- packageList (GetPackageList psid)
    --   ls <- forP pset $ \pkgid@(PackageIdentifier n v) -> do
    --     BuildName bn <- getBuildName (psid, pkgid)
    --     let url = "https://build.opensuse.org/package/show/devel:languages:haskell/" ++ bn
    --     return $ intercalate "," [ show (display n), show (display v), show url]
    --   writeFile' out (intercalate "\n" ls)

    -- buildDir </> "cabal-*.config" %> \out -> do
    --   alwaysRerun
    --   let lts = drop 6 (takeBaseName out)
    --       url = "https://www.stackage.org/" ++ lts ++"/cabal.config"
    --   command_ [FileStdout out] "curl" ["-L", "-s", url]

    -- "tools/cabal2obs/Config/*/Stackage.hs" %> \out -> do
    --   let dirname = takeBaseName (takeDirectory out)
    --       psid = case dirname of
    --                "Nightly"     -> "nightly"
    --                'L':'T':'S':x -> "lts-" ++ x
    --                _              -> error ("invaid cabal2obs config path " ++ show dirname)
    --   buf <- readFile' (buildDir </> "cabal-" ++ psid <.> "config")
    --   deps <- runP stackageConfig buf
    --   writeFileChanged out (mkStackagePackageSetSourcefile dirname deps)

    -- phony "update" $ need
    --   [ "tools/cabal2obs/Config" </> getConfigDirname psid </> "Stackage.hs" | psid <- knownPackageSets ]

    -- phony "fetch" $ do
    --   pkgs' <- mapM (packageList . GetPackageList) knownPackageSets
    --   let pkgs :: [PackageIdentifier]
    --       pkgs = sort (nub (concat pkgs'))
    --   need [ homeDir </> ".cabal/packages/hackage.haskell.org" </> display pn </> display v </> display pid <.> "tar.gz"
    --        | pid@(PackageIdentifier pn v) <- pkgs
    --        ]

verifyLicense :: Monad m => String -> m ()
verifyLicense "SUSE-Public-Domain" = return ()
verifyLicense lic = case eitherParsec lic of
  Left msg -> fail ("invalid license expression " ++  lic ++ "\n" ++ msg)
  Right l -> let lic' = prettyShow (l :: License)
             in unless (lic == lic') $
               fail ("license " ++ lic ++ " doesn't match expected " ++ lic')

-- mkStackagePackageSetSourcefile :: String -> [Dependency] -> String
-- mkStackagePackageSetSourcefile vers deps = unlines
--   [ "{-# LANGUAGE OverloadedStrings #-}"
--   , "{-# OPTIONS_GHC -fno-warn-deprecations #-}"
--   , ""
--   , "module Config." ++ vers ++ ".Stackage where"
--   , ""
--   , "import Orphans ( )"
--   , "import Distribution.Package"
--   , ""
--   , "stackage :: [Dependency]"
--   , "stackage ="
--   , "  [ " ++ intercalate "\n  , " (map (show . display) deps)
--   , "  ]"
--   ]
