{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import MyCabal

import Data.Map.Strict as Map
import Data.Set as Set
import Data.String
import Development.Shake.Classes
import GHC.Generics ( Generic )

newtype BuildName = BuildName { unBuildName :: String }
  deriving (Show, Eq, Ord, Hashable, NFData, Binary, Generic)

instance IsString BuildName where
  fromString = BuildName

newtype PackageSetId = PackageSetId { unPackageSetId :: String }
  deriving (Show, Eq, Ord, Hashable, NFData, Binary, Generic)

instance IsString PackageSetId where
  fromString = PackageSetId

type Constraint = Dependency

type ConstraintSet = Map PackageName VersionRange

type PackageSet = Map PackageName Version

data PackageSetConfig = PackageSetConfig
  { compiler         :: CompilerId
  , packageSet       :: PackageSet
  , flagAssignments  :: Map PackageName FlagAssignment
  , forcedExectables :: Set PackageName
  , corePackages     :: PackageSet
  }
  deriving (Show, Binary, Generic, Eq, Hashable, NFData)
