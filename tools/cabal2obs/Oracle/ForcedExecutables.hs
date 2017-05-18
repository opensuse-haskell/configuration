{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Oracle.ForcedExecutables where

import Orphans ( )
import Types
import Config

import Development.Shake
import Development.Shake.Classes
import Distribution.Package

newtype GetForcedExes = GetForcedExes PackageSetId
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

getForcedExes :: Rules (GetForcedExes -> Action [PackageName])
getForcedExes = addOracle $ \(GetForcedExes psid) -> return (forcedExectables (getConfig psid))
