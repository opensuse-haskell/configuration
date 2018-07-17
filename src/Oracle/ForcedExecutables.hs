{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Oracle.ForcedExecutables where

import Orphans ( )
import Types
import Config

import Development.Shake
import Development.Shake.Classes
import Distribution.Package

newtype GetForcedExes = GetForcedExes PackageSetId
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

type instance RuleResult GetForcedExes = [PackageName]

getForcedExes :: Rules (GetForcedExes -> Action [PackageName])
getForcedExes = addOracle $ \(GetForcedExes psid) -> return (forcedExectables (getConfig psid))
