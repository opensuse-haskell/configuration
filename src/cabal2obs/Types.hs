{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Map.Strict as Map
import Data.Set as Set
import Data.String
import Development.Shake.Classes
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import GHC.Generics ( Generic )
import Orphans ( )

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
  }
  deriving (Show, Binary, Generic, Eq, Hashable, NFData)
