{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Oracle.ForcedExecutables ( addForcedExecutableOracle ) where

import Config ( PackageSetSpec(forcedExecutables), packageSetSpec )
import MyCabal

import qualified Data.Set as Set
import Development.Shake
import Development.Shake.Classes

-- | Key for the oracle that tracks whether a package is forced to be packaged
-- as an executable, even though it has a library.
newtype IsForcedExecutable = IsForcedExecutable PackageName
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult IsForcedExecutable = Bool

-- | Register an oracle that tells whether a package is listed in our
-- configuration as a forced executable. Rules that query it are re-built when
-- a package is added to or removed from that list.

addForcedExecutableOracle :: Rules (PackageName -> Action Bool)
addForcedExecutableOracle = do
  isForced <- addOracle $ \(IsForcedExecutable pn) -> Set.member pn . forcedExecutables <$> packageSetSpec
  pure (isForced . IsForcedExecutable)
