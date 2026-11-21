{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Oracle.PackageSet ( addPackageSetOracle, addAvailablePackagesOracle ) where

import Config ( PackageSetSpec(corePackages, packageSet), packageSetSpec )
import Types

import qualified Data.Map.Strict as Map
import Development.Shake
import Development.Shake.Classes

-- | Key for the oracle that tracks our package set.
newtype ThePackageSet = ThePackageSet ()
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult ThePackageSet = PackageSet

-- | Register an oracle that gives the package set we generate builds for.
-- Rules that query it are re-built whenever the package set changes in any
-- way, so rules that care only about some aspects of it should query a more
-- specific oracle instead, e.g. 'addAvailablePackagesOracle'.

addPackageSetOracle :: Rules (Action PackageSet)
addPackageSetOracle = do
  getPackageSet <- addOracle $ \(ThePackageSet ()) -> packageSet <$> packageSetSpec
  pure (getPackageSet (ThePackageSet ()))

-- | Key for the oracle that tracks which packages are available to satisfy
-- dependencies.
newtype AvailablePackages = AvailablePackages ()
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult AvailablePackages = PackageNameSet

-- | Register an oracle that gives the names of all packages that are
-- available to satisfy dependencies, i.e. those in our package set plus the
-- compiler's core packages. Rules that query it are re-built when a package is
-- added to or removed from that set. Changes of package versions don't affect
-- the result.

addAvailablePackagesOracle :: Rules (Action PackageNameSet)
addAvailablePackagesOracle = do
  getAvailablePackages <- addOracle $ \(AvailablePackages ()) ->
    (\spec -> Map.keysSet (packageSet spec `Map.union` corePackages spec)) <$> packageSetSpec
  pure (getAvailablePackages (AvailablePackages ()))
