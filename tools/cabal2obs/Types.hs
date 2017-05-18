{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Development.Shake.Classes
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import GHC.Generics ( Generic )

newtype BuildName = BuildName { unBuildName :: String }
  deriving (Show, Eq, Ord, Hashable, NFData, Binary, Generic)

newtype PackageSetId = PackageSetId { unPackageSetId :: String }
  deriving (Show, Eq, Ord, Hashable, NFData, Binary, Generic)

type Constraint = Dependency

data PackageSetConfig = PackageSetConfig
  { compiler :: CompilerId
  , stackagePackages :: [Constraint]
  , extraPackages :: [Constraint]
  , bannedPackages :: [PackageName]
  , flagAssignments :: [(PackageName,FlagAssignment)]
  , forcedExectables :: [PackageName]
  }
  deriving (Show, Binary, Generic)
