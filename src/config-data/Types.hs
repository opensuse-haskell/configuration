{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import MyCabal

import Data.Map.Strict as Map
import Data.String
import Development.Shake.Classes
import GHC.Generics ( Generic )

newtype BuildName = BuildName { unBuildName :: String }
  deriving (Show, Eq, Ord, Hashable, NFData, Binary, Generic)

instance IsString BuildName where
  fromString = BuildName

type Constraint = Dependency

type ConstraintSet = Map PackageName VersionRange

type PackageSet = Map PackageName Version

-- | The name of an attribute in nixpkgs, e.g. @"libyaml"@.
type NixpkgsAttribute = String
