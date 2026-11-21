{-# LANGUAGE RecordWildCards #-}

-- | The configuration of our package set, read from a YAML file. This is the
-- only module of the configuration that is visible outside of the cabal2obs
-- library; the Config.* modules are internal.

module Config
  ( -- * Specification of a package set
    PackageSetSpec(..), readPackageSetSpec, updatePackageSetSpecFile
    -- * Tools
  , getPackageSetName, ppPackageSet, ppConstraintSet, updateTargetPackages
  ) where

import Config.PackageSetSpec
import Config.PackageSetSpecRaw
import Config.Tools
import MyCabal

import Control.Monad ( unless )
import Control.Monad.IO.Class
import Data.Char ( isSpace )
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Directory ( renameFile )
import System.IO ( readFile' )

-- | Read the specification of a package set from the given YAML file. Fails
-- if the file can't be read or if its contents are inconsistent, e.g. if a list contains duplicates, a package
-- identifier has no version, a flag is assigned more than once for the same
-- package, or a core package is listed in the package set.

readPackageSetSpec :: (MonadIO m, MonadFail m) => FilePath -> m PackageSetSpec
readPackageSetSpec path = do
  let context what = what <> " in " <> path
  PackageSetSpecRaw{..} <- readPackageSetSpecRaw path
  let compiler = rawCompiler
  packageSetName        <- getPackageSetName compiler
  corePackages          <- makeMap (context "core-packages") [ (pkgName p, pkgVersion p) | p <- rawCorePackages ]
  packageSet            <- makeMap (context "package-set") [ (pkgName p, pkgVersion p) | p <- rawPackageSet ]
  targetPackages        <- makeMap (context "target-packages") [ (pn, vr) | Dependency pn vr _ <- rawTargetPackages ]
  allowNewer            <- makeMap (context "allow-newer") =<<
                             traverse (\(pn, deps) -> (,) pn <$> makeSet (context ("allow-newer of " <> unPackageName pn)) deps) rawAllowNewer
  flags                 <- makeMap (context "flags") rawFlags
  forcedExecutables     <- makeSet (context "forced-executables") rawForcedExecutables
  pkgConfigDependencies <- makeSet (context "pkg-config-dependencies") rawPkgConfigDependencies
  let unversioned = [ "package identifiers without a version in " <> context section <> ": " <> List.intercalate ", " names
                    | (section, pids) <- [("core-packages", rawCorePackages), ("package-set", rawPackageSet)]
                    , let names = [ unPackageName (pkgName p) | p <- pids, null (versionNumbers (pkgVersion p)) ]
                    , not (null names)
                    ]
  unless (null unversioned) $
    fail (List.intercalate "; " unversioned)
  let duplicateFlags = [ unPackageName pn <> " (" <> List.intercalate ", " (unFlagName <$> fs) <> ")"
                       | (pn, fa) <- rawFlags, let fs = findDuplicateFlagAssignments fa, not (null fs) ]
  unless (null duplicateFlags) $
    fail ("flags assigned more than once in " <> context "flags" <> ": " <> List.intercalate ", " duplicateFlags)
  let corePackagesInPackageSet = Map.keysSet packageSet `Set.intersection` Map.keysSet corePackages
  unless (Set.null corePackagesInPackageSet) $
    fail ("core packages listed in " <> context "package-set" <> ": " <> List.intercalate ", " (unPackageName <$> Set.toList corePackagesInPackageSet))
  pure PackageSetSpec{..}

-- | Write the package set and the target packages of the given specification
-- into the given YAML file, replacing the contents of its @package-set@ and
-- @target-packages@ sections. All other lines of the file, including its
-- comments and formatting, stay as they are. Comments inside the two sections
-- are lost.
--
-- The updated file is first written to @<file>.new@ and read back. It
-- replaces the original only if reading it gives the given specification;
-- otherwise this function fails and leaves @<file>.new@ for inspection.

updatePackageSetSpecFile :: (MonadIO m, MonadFail m) => FilePath -> PackageSetSpec -> m ()
updatePackageSetSpecFile path spec = do
  old <- liftIO (readFile' path)
  let byName :: [(PackageName, a)] -> [(PackageName, a)]
      byName = List.sortOn (lowercase . unPackageName . fst)
      packages = [ display (PackageIdentifier pn v) | (pn, v) <- byName (Map.toList (packageSet spec)) ]
      targets  = [ display (PackageVersionConstraint pn vr) | (pn, vr) <- byName (Map.toList (targetPackages spec)) ]
  new <- replaceSection "package-set" packages old >>= replaceSection "target-packages" targets
  let tmp = path <> ".new"
  liftIO (writeFile tmp new)
  spec' <- readPackageSetSpec tmp
  unless (spec' == spec) $
    fail (tmp <> ": the updated file doesn't specify the expected package set")
  liftIO (renameFile tmp path)
  where
    -- Replace the items of the section with the given key. The section ends
    -- before the next line that starts in the first column; blank lines at
    -- its end are kept.
    inSection :: String -> Bool
    inSection (c : _) = isSpace c
    inSection []      = True

    replaceSection :: MonadFail m => String -> [String] -> String -> m String
    replaceSection key items txt =
      case break ((== key <> ":") . List.dropWhileEnd isSpace) (lines txt) of
        (_, []) -> fail (path <> ": no section " <> key)
        (before, header : rest) -> do
          let (body, after) = span inSection rest
              blanks = reverse (takeWhile null (reverse body))
          pure (unlines (before ++ [header] ++ (("  - " <>) <$> items) ++ blanks ++ after))
