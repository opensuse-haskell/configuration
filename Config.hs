{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Compiler
import Data.Map ( Map )
import GHC.Generics ( Generic )
import Development.Shake.Classes
import Orphans ()

newtype BuildName = BuildName { unBuildName :: String }
  deriving (Show, Eq, Ord)

newtype PackageSetId = PackageSetId { unPackageSetId :: String }
  deriving (Show, Eq, Ord)

newtype Revision = Revision { unRevision :: Int }
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

data BuildDescription = BuildDescription
  { pid :: PackageIdentifier
  , prv :: Revision
  , hasLib :: Bool
  , hasExe :: [FilePath]
  , forcedExe :: Bool
  , flags :: FlagAssignment
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable BuildDescription
instance NFData BuildDescription
instance Binary BuildDescription

instance Package BuildDescription where
  packageId = pid

data PackageSet = PackageSet
  { pkgs :: Map BuildName BuildDescription
  , compiler :: CompilerId
  }
  deriving (Show, Eq, Ord)

type Config = Map PackageSetId PackageSet
