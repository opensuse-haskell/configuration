module Config where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Compiler
import Data.Map

newtype BuildName = BuildName { unBuildName :: String }
  deriving (Show, Eq, Ord)

newtype PackageSetId = PackageSetId { unPackageSetId :: String }
  deriving (Show, Eq, Ord)

newtype Revision = Revision { unRevision :: Int }
  deriving (Show, Eq, Ord)

data BuildDescription = BuildDescription
  { pid :: PackageIdentifier
  , prv :: Revision
  , hasLib :: Bool
  , executables :: [FilePath]
  , forcedExe :: Bool
  , flags :: FlagAssignment
  }
  deriving (Show, Eq, Ord)

instance Package BuildDescription where
  packageId = pid

data PackageSet = PackageSet
  { pkgs :: Map BuildName BuildDescription
  , compiler :: CompilerId
  }
  deriving (Show, Eq, Ord)

type Config = Map PackageSetId PackageSet
