{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Oracle.PackageSet ( addAvailablePackagesOracle ) where

import MyCabal

import Data.Set ( Set )
import Development.Shake
import Development.Shake.Classes

-- | Key for the oracle that tracks which packages are available to satisfy
-- dependencies.
newtype AvailablePackages = AvailablePackages ()
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult AvailablePackages = Set PackageName

-- | Register an oracle that gives the given set of package names. Rules that
-- query it are re-built when a package is added to or removed from that set.
-- Changes of package versions don't affect the result.

addAvailablePackagesOracle :: Set PackageName -> Rules (Action (Set PackageName))
addAvailablePackagesOracle pkgs = do
  getAvailablePackages <- addOracle $ \(AvailablePackages ()) -> pure pkgs
  pure (getAvailablePackages (AvailablePackages ()))
