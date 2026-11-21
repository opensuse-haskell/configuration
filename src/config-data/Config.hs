{-# LANGUAGE RecordWildCards #-}

-- | The configuration of our package set, read from a YAML file. This is the
-- only module of the configuration that is visible outside of the config-data
-- library.

module Config
  ( -- * Specification of a package set
    PackageSetSpec(..), readPackageSetSpec
    -- * Tools
  , getPackageSetName, ppPackageSet, ppConstraintSet, updateTargetPackages
  ) where

import Config.PackageSetSpec
import Config.PackageSetSpecRaw
import Config.Tools
import MyCabal

import Control.Monad ( unless )
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Read the specification of our package set from
-- @conf/default-package-set.yaml@. Fails if the file can't be read or if its
-- contents are inconsistent, e.g. if a list contains duplicates, a package
-- identifier has no version, a flag is assigned more than once for the same
-- package, or a core package is listed in the package set.

readPackageSetSpec :: IO PackageSetSpec
readPackageSetSpec = do
  let path = "conf/default-package-set.yaml"
      context what = what <> " in " <> path
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
