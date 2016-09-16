module Main ( main ) where

import Oracle
import Orphans ()
import ParseUtils
import Types

import Control.Monad
import Data.List
import Development.Shake
import Development.Shake.FilePath
import Distribution.Package
import Distribution.Text
import System.Directory
import System.Environment
import System.Exit

main :: IO ()
main = do
  homeDir <- System.Environment.getEnv "HOME"
  let buildDir = "_build"
      configDir = "config"
      hackageDir = "hackage"

      shopts = shakeOptions
               { shakeFiles = buildDir
               , shakeProgress = progressSimple
               }

  shakeArgs shopts $ do

    -- Register our custom oracles to query the configuration files.
    getCabal <- hackageDB hackageDir
    resolver <- resolveConstraint hackageDir
    packageList <- getPackageList configDir resolver
    forcedExes <- getForcedExes configDir
    compilerId <- getCompiler configDir
    flagAssignments <- getFlagAssignments configDir

    getBuildName <- addOracle $ \(psid@(PackageSetId _), pkgid@(PackageIdentifier _ _)) -> do
      fexeset <- forcedExes (GetForcedExes psid)
      cabal <- getCabal pkgid
      let forceExe = packageName pkgid `elem` fexeset
          prefix | forceExe || not (hasLibrary cabal)  = ""
                 | otherwise                           = "ghc-"
      return $ BuildName (prefix ++ unPackageName (packageName pkgid))

    pkgidFromPath <- addOracle $ \(PackageSetId psid, BuildName bn) -> do
      pset <- askOracle (GetPackageList (PackageSetId psid)) :: Action [PackageIdentifier]
      id1 <- case find (\(PackageIdentifier (PackageName n) _) -> n == bn) pset of
               Nothing -> return Nothing
               Just pkgid -> do BuildName bn' <- getBuildName (PackageSetId psid, pkgid)
                                return (if bn == bn' then Just pkgid else Nothing)
      id2 <- case stripPrefix "ghc-" bn of
               Nothing -> return Nothing
               Just bn' -> case find (\(PackageIdentifier (PackageName n) _) -> n == bn') pset of
                             Nothing -> return Nothing
                             Just pkgid -> do BuildName bn'' <- getBuildName (PackageSetId psid, pkgid)
                                              return (if bn == bn'' then Just pkgid else Nothing)
      case (id1,id2) of
        (Nothing, Nothing) -> fail $ "no package called " ++ show bn ++ " configured in " ++ psid ++ " package set"
        (Just pkgid, _)    -> return pkgid
        (_, Just pkgid)    -> return pkgid

    -- Compute all configured builds and register the required targets.
    action $ do
      psets <- fmap PackageSetId <$> getDirectoryDirs configDir
      targets <- forP psets $ \psid -> do
        pset <- packageList (GetPackageList psid)
        fexeset <- forcedExes (GetForcedExes psid)
        forP pset $ \pkgid@(PackageIdentifier pn@(PackageName n) _) -> do
          cabal <- getCabal pkgid
          let isForcedExe = pn `elem` fexeset
              isExe = isForcedExe || not (hasLibrary cabal)
              bn = (if isExe then "" else "ghc-") ++ n
              pkgDir = buildDir </> unPackageSetId psid </> bn
          return [ pkgDir </> bn <.> "spec", pkgDir </> (display pkgid <.> "tar.gz") ]
      need (concat (concat targets))

    -- Pattern target to trigger source tarball downloads with "cabal get". We
    -- prefer this over direct downloading becauase "cabal" acts as a cache for
    -- us, too.
    homeDir </> ".cabal/packages/hackage.haskell.org/*/*/*.tar.gz" %> \out -> do
      exists <- liftIO $ System.Directory.doesFileExist out
      -- The explicit test for existence is necessary because shake will
      -- re-build existing files if its internal database does not know how the
      -- file was created.
      unless exists $ do
        let pkgid = dropExtension (takeBaseName out)
        command_ [] "cabal" ["fetch", "-v0", "--no-dependencies", "--", pkgid]

    -- Pattern rule that copies the required source tarballs from cabal's
    -- internal cache into our build tree.
    buildDir </> "*/*/*.tar.gz" %> \out -> do
      let pkgid = dropExtension (takeBaseName out)
      PackageIdentifier (PackageName n) v <- parseText "package id" pkgid
      liftIO $ removeFiles (takeDirectory out) ["*.tar.gz"]
      copyFile'
        (homeDir </> ".cabal/packages/hackage.haskell.org" </> n </> display v </> pkgid <.> "tar.gz")
        out

    -- Pattern rule that copies the required source tarballs from cabal's
    -- internal cache into our build tree.
    buildDir </> "*/*/*.spec" %> \out -> do
       let [_,psid',bn',_] = splitDirectories out
           psid = PackageSetId psid'
           bn = BuildName bn'
       pkgid@(PackageIdentifier (PackageName n) v) <- pkgidFromPath (psid,bn)
       let isExe = n == bn'
           pkgDir = takeDirectory out
       cid <- compilerId (GetCompiler psid)
       fas <- flagAssignments (GetFlagAssignments psid)
       cabal <- getCabal pkgid
       let rev = packageRevision cabal
       liftIO $ removeFiles pkgDir ["*.cabal"]
       when (rev > 0) $
          copyFileChanged (cabalFilePath hackageDir pkgid) (pkgDir </> show rev <.> "cabal")
       -- cabal-rpm breaks if these files exist when it's run.
       liftIO $ removeFiles pkgDir ["*.spec", display pkgid]
       command_ [Cwd pkgDir, EchoStdout False]
                "../../../tools/cabal-rpm/dist/build/cabal-rpm/cabal-rpm"
                ([ "--strict"
                 , "--distro=SUSE"
                 , "--compiler=" ++ display cid
                 ] ++ ["-b" | isExe] ++ maybe [] words (lookup n fas) ++
                 [ "spec"
                 , display pkgid
                 ])
       command_ [] "spec-cleaner" ["-i", out]
       patches <- getDirectoryFiles "" [ "patches/common/" ++ n ++ "/*.patch"
                                       , "patches/" ++ psid' ++ "/" ++ n ++ "/*.patch"
                                       ]
       need patches
       forM_ (sort patches) $ \p ->
         command_ [] "patch" ["--no-backup-if-mismatch", "--force", out, p]
       Exit c1 <- command [] "grep" ["--silent", "-E", "^License:.*Unknown", out]
       when (c1 == ExitSuccess) $ fail "invalid license type 'Unknown'"

       let versionString = unwords $ ["version", display v] ++
                                     if rev==0  then [] else ["revision", show rev]
       Exit c2 <- command [] "grep" ["-q", "-s", "-F", "-e", versionString, out -<.> "changes"]
       when (c2 /= ExitSuccess) $
         command_ [Cwd pkgDir] "osc" ["vc", "-m", "Update to " ++ versionString ++ " with cabal2obs."]

   -- Pattern rule to copy revised Cabal files into the package directory.
   -- buildDir </> "*/*/*.cabal" %> \out -> undefined

--     action $ do
--       ls <- forM (nub (concat packageSets)) $ \p@(PackageIdentifier (PackageName n) v) -> do
--         SusePackageDescription _ isExe <- getSusePkgDescription p
--         let pn = (if isExe then "" else "ghc-") ++ n
--             pv = display v
--             url = "https://build.opensuse.org/package/show/devel:languages:haskell/" ++ pn
--         return $ show n ++ "," ++ show pv ++ "," ++ show url
--       liftIO $ writeFile (buildDir </> "package-list.csv") (intercalate "\n" ls)
--
--     forM_ (zip stackageVersions packageSets) $ \(stackageVersion,stackage) ->
--       forM_ stackage $ \p@(PackageIdentifier (PackageName pn) v) -> do
--         let forcedExe = pn `elem` forcedExecutablePackages
--             isExe = forcedExe || not (isLibrary hackage p)
--             rv = hackageRevision hackage p
--             pkgName = (if isExe then "" else "ghc-") ++ pn
--             pkgDir = "_build" </> stackageVersion </> pkgName
--             spec = pkgDir </> pkgName <.> "spec"
--             pv = display v
--             pid = display p
--             tar = pkgDir </> pid <.> "tar.gz"
--             tarsrc = homeDir </> ".cabal/packages/hackage.haskell.org" </> pn </> pv </> pid <.> "tar.gz"
--             editedCabalFile = pkgDir </> show rv <.> "cabal"
--             editedCabalFileUrl = "https://hackage.haskell.org/package/" ++ pid ++ "/revision/" ++ show rv ++ ".cabal"
--
--         want [spec, tar]
--
--         tar %> \out -> do
--           need [tarsrc]
--           b <- liftIO $ System.Directory.doesFileExist tar
--           unless b $
--             bash [ "rm -f " ++ pkgDir ++ "/*.tar.gz", "cp " ++ tarsrc ++ " " ++ out ]
--
--         when (rv > 0) $ do
--           want [editedCabalFile]
--           editedCabalFile %> \_ -> do
--             SusePackageDescription _ _ <- getSusePkgDescription p
--             bash ["cd " ++ pkgDir, "rm -f *.cabal", "wget -q " ++ editedCabalFileUrl]
--
--         spec %> \_ -> do
--           SusePackageDescription _ _ <- getSusePkgDescription p
--           when (rv > 0) $
--             need [editedCabalFile]
--           compiler <- getCompilerVersion (StackageVersion stackageVersion)
--           patches <- sort <$> getDirectoryFiles "" [ "patches/common/" ++ pn ++ "/*.patch"
--                                                    , "patches/" ++ stackageVersion ++ "/" ++ pn ++ "/*.patch"
--                                                    ]
--           let clvid = unwords ["version", pv, "revision", show rv]
--           need patches
--           flags <- getFlagAssignment (PackageName pn)
--           bash $ [ "cd " ++ pkgDir
--                  , "rm -f *.spec"
--                  , "rm -rf " ++ display p       -- cabal-rpm fails if such a directory exists
--                  , "../../../tools/cabal-rpm/dist/build/cabal-rpm/cabal-rpm --strict --compiler=" ++
--                    compiler ++ (if forcedExe then " -b " else " ") ++ "--distro=SUSE " ++
--                    flags ++ " " ++
--                    "spec " ++ pid ++ " >/dev/null"
--                  , "spec-cleaner -i " ++ pkgName <.> "spec"
--                  , "grep -q -s -F -e '" ++ clvid ++ "' " ++ pkgName <.> "changes" ++
--                    "|| osc vc -m 'Update to " ++ clvid ++ " with cabal2obs.'"
--                  ] ++
--                  [ "patch --no-backup-if-mismatch --force <../../../" ++ pt | pt <- patches ] ++
--                  [ "if grep >&2 -E '^License:.*Unknown' " ++ pkgName <.> "spec" ++ "; then exit 1; fi"
--                  ] ++ if rv == 0 then ["rm -f ?.cabal"] else []
