{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Oracle.Flags ( addFlagAssignmentOracle ) where

import Config ( PackageSetSpec(flags), packageSetSpec )
import MyCabal

import qualified Data.Map.Strict as Map
import Development.Shake
import Development.Shake.Classes

-- | Key for the oracle that tracks the flag assignment of a package.
newtype FlagsFor = FlagsFor PackageName
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult FlagsFor = FlagAssignment

-- | Register an oracle that gives the flag assignment of a package from our
-- configuration, defaulting to no assignments. Rules that query it are
-- re-built when a package's flags change in the configuration.

addFlagAssignmentOracle :: Rules (PackageName -> Action FlagAssignment)
addFlagAssignmentOracle = do
  getFlags <- addOracle $ \(FlagsFor pn) -> pure (Map.findWithDefault mempty pn (flags packageSetSpec))
  pure (getFlags . FlagsFor)
