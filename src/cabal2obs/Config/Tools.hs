{-# LANGUAGE RecordWildCards #-}

module Config.Tools ( checkConsistency, resolveConstraints ) where

import Config.Flags
import Types
import MyCabal

import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map.Strict ( toList )
import qualified Data.Set as Set


-- | Sanity-check a package set configuration before it is used to generate
-- builds. Returns the configuration unchanged if it is consistent; otherwise
-- it calls 'fail', which aborts the Shake build with an error message.
--
-- The check detects packages that appear both in the package set (i.e. in the
-- compiler's @constraintList@) and in its list of core packages. Core packages
-- are bundled with GHC itself and are provided by the @ghc@ build, so
-- generating a separate @ghc-<name>@ build for one of them would produce a
-- second, conflicting copy of that library. The error message names every
-- offending package.
--
-- TODO: Detect at compile-time or run-time if constraintList contains
-- duplicate entries.

checkConsistency :: MonadFail m => PackageSetConfig -> m PackageSetConfig
checkConsistency pset@PackageSetConfig {..} = do
  let corePackagesInPackageSet = Map.keysSet packageSet `Set.intersection` Map.keysSet corePackages
  unless (Set.null corePackagesInPackageSet) $
    fail ("core packages listed in package set: " <> List.intercalate ", " (unPackageName <$> Set.toList corePackagesInPackageSet))
  pure pset

resolveConstraints :: ConstraintSet -> String
resolveConstraints targetPackages = unwords ["cabal", "install", "--dry-run", "--minimize-conflict-set", constraints, flags, pkgs]
  where
    pkgs = unwords (display <$> Map.keys targetPackages)
    constraints = "--constraint=" <> List.intercalate " --constraint=" (show <$> environment)
    environment = display . uncurry PackageVersionConstraint <$> toList targetPackages
    flags = unwords [ "--constraint=" <> show (unwords [unPackageName pn, flags'])
                    | pn <- Map.keys targetPackages
                    , Just flags' <- [lookup (unPackageName pn) flagList]
                    ]
