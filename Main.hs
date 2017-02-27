module Main ( main ) where

import Cabal2Spec
import Oracle
import Orphans ()
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
import Distribution.PackageDescription.Configuration
import Distribution.System
import Distribution.Text
import Distribution.Version
import System.Directory
import System.Environment

main :: IO ()
main = do
  homeDir <- System.Environment.getEnv "HOME"
  let buildDir = "_build"
      configDir = "config"
      hackageDir = "hackage"

      shopts = shakeOptions
               { shakeFiles = buildDir
               , shakeProgress = progressSimple
               , shakeThreads = 0       -- autodetect the number of available cores
               , shakeVersion = "2"     -- version of the build rules, bump to trigger full re-build
               }

  shakeArgs shopts $ do

    -- Register our custom oracles to query the configuration files.
    getCabal <- hackageDB hackageDir
    resolver <- resolveConstraint hackageDir
    packageList <- getPackageList configDir resolver
    forcedExes <- getForcedExes configDir
    compilerId <- getCompiler configDir
    flagAssignments <- getFlagAssignments configDir

    getFlags <- addOracle $ \(psid@(PackageSetId _), PackageName n) -> do
      fas <- flagAssignments (GetFlagAssignments psid)
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
      need ((buildDir </> "packages.csv") : concat (concat targets))
      -- get rid of *.orig and *.rej files created by patch(1).
      removeFilesAfter buildDir ["*/*/*.orig", "*/*/*.rej"]

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
      liftIO $ removeFiles (takeDirectory out) ["*.tar.gz"]
      let pkgid = dropExtension (takeBaseName out)
      PackageIdentifier (PackageName n) v <- parseText "package id" pkgid
      if n == "git-annex"
         then command_ [FileStdout out ] "curl"
                       [ "-L", "--silent", "--show-error", "--"
                       , "https://github.com/joeyh/git-annex/archive/" ++ display v ++".tar.gz"
                       ]
         else copyFile' (homeDir </> ".cabal/packages/hackage.haskell.org" </> n </> display v </> pkgid <.> "tar.gz") out

    -- Pattern rule that generates the package's spec file.
    buildDir </> "*/*/*.spec" %> \out -> do
      let [_,psid',bn',_] = splitDirectories out
          psid = PackageSetId psid'
          bn = BuildName bn'
      pkgid@(PackageIdentifier (PackageName n) v) <- pkgidFromPath (psid,bn)
      let isExe = n == bn'
          pkgDir = takeDirectory out
      cid <- compilerId (GetCompiler psid)
      fa <- getFlags (psid, PackageName n)
      cabal <- getCabal pkgid
      let rev = packageRevision cabal
      if (rev > 0)
         then copyFile' (cabalFilePath hackageDir pkgid) (pkgDir </> n <.> "cabal")
         else liftIO (removeFiles pkgDir ["*.cabal"])

      -- need [pkgDir </> display pkgid <.> "tar.gz"]
      liftIO $ removeFiles pkgDir [display pkgid]
      command_ [Cwd pkgDir] "cabal" ["get", display pkgid]

      case finalizePackageDescription fa (const True) (Platform X86_64 Linux) (unknownCompilerInfo cid NoAbiTag) [] cabal of
        Left missing -> fail ("finalizePackageDescription: " ++ show missing)
        Right (desc,_) -> do putNormal $ show ("createSpecFile", out, (pkgDir </> display pkgid </> display pkgid <.> "cabal"), isExe, fa)
                             liftIO $ void $ createSpecFile out (pkgDir </> display pkgid </> display pkgid <.> "cabal") desc isExe fa

      -- command_ [Cwd pkgDir, EchoStdout False]
      --          "../../../tools/cabal-rpm/dist/build/cabal-rpm/cabal-rpm"
      --          ([ "--strict"
      --           , "--distro=SUSE"
      --           , "--compiler=" ++ display cid
      --           ] ++ ["-b" | isExe] ++ words fa ++
      --           [ "spec"
      --           , display pkgid
      --           ])
      command_ [Cwd "tools/spec-cleaner"] "python3" ["-m", "spec_cleaner", "-i", "../.." </> out]

      patches <- getDirectoryFiles "" [ "patches/common/" ++ n ++ "/*.patch"
                                      , "patches/" ++ psid' ++ "/" ++ n ++ "/*.patch"
                                      ]
      need patches
      forM_ (sortBy (compare `on` takeFileName) patches) $ \p -> do
        command_ [] "patch" ["--no-backup-if-mismatch", "--force", out, p]
        command_ [Cwd "tools/spec-cleaner"] "python3" ["-m", "spec_cleaner", "-i", "../.." </> out]
      Stdout buf <- command [] "sed" ["-n", "-e", "s/^License: *//p", out]
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
      ls <- forP pset $ \pkgid@(PackageIdentifier (PackageName n) v) -> do
        BuildName bn <- getBuildName (psid, pkgid)
        let url = "https://build.opensuse.org/package/show/devel:languages:haskell/" ++ bn
        return $ intercalate "," [ show n, show (display v), show url]
      writeFile' out (intercalate "\n" ls)


mkChangeEntry :: String -> String -> IO String
mkChangeEntry version email = do
  ts <- formatTime defaultTimeLocale "%a %b %_d %H:%M:%S %Z %Y" <$> getCurrentTime
  return $ unlines
    [ "-------------------------------------------------------------------"
    , unwords [ ts, "-", email ]
    , ""
    , unwords [ "- Update to", version, "with cabal2obs." ]
    , ""
    ]

verifyLicense :: Monad m => String -> m ()
verifyLicense "SUSE-Public-Domain" = return ()
verifyLicense license
  | [l] <- parseExpression license = when (prettyLicenseExpression l /= license) $ fail $
                                       unwords [ "license expression"
                                               , license
                                               , "doesn't match expected"
                                               , prettyLicenseExpression l
                                               ]
  | otherwise                      = fail (unwords ["invalid license expression", show license])
