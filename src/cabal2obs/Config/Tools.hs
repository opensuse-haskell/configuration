module Config.Tools ( checkConsistency ) where

import Config.Compiler
import Types
import MyCabal

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set


-- | Sanity-check the package set against the compiler's core packages before
-- it is used to generate builds. Returns the package set unchanged if it is
-- consistent; otherwise it aborts with an 'error'.
--
-- The check detects packages that appear both in the package set (i.e. in
-- @packageSet@) and in the list of core packages, 'ghcCorePackages'. Core packages
-- are bundled with GHC itself and are provided by the @ghc@ build, so
-- generating a separate @ghc-<name>@ build for one of them would produce a
-- second, conflicting copy of that library. The error message names every
-- offending package.
--
-- TODO: Detect at compile-time or run-time if packageSet contains
-- duplicate entries.

checkConsistency :: PackageSet -> PackageSet
checkConsistency packageSet
  | Set.null corePackagesInPackageSet = packageSet
  | otherwise = error ("core packages listed in package set: " <> List.intercalate ", " (unPackageName <$> Set.toList corePackagesInPackageSet))
  where
    corePackagesInPackageSet = Map.keysSet packageSet `Set.intersection` Map.keysSet ghcCorePackages
