module Config
  ( compiler
  , packageSet
  , flagAssignments
  , forcedExectables
  , corePackages
  ) where

import Config.Compiler
import Config.Flags
import Config.ForcedExecutables
import Config.PkgSet
import MyCabal
import Types

import Data.Map.Strict ( Map, fromList )
import Data.Set ( Set )

compiler :: CompilerId
compiler = ghcCompiler

flagAssignments :: Map PackageName FlagAssignment
flagAssignments = fromList (readFlagAssignents flagList)

forcedExectables :: Set PackageName
forcedExectables = forcedExectableNames

corePackages :: PackageSet
corePackages = ghcCorePackages
