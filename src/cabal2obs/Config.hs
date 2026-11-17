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
import Config.Tools
import MyCabal
import Oracle.Hackage ( )
import Types

import Control.Monad
import Data.Map.Strict ( Map, fromList, toList )
import Data.Set ( Set )
import Development.Shake

compiler :: CompilerId
compiler = ghcCompiler

-- | Resolve 'constraintList' against Hackage into concrete package versions.
packageSet :: Action PackageSet
packageSet = do
  pset <- fromList <$>
            forM (toList constraintList) (\(pn,vr) ->
              (,) pn <$> askOracle (PackageVersionConstraint pn vr))
  checkConsistency pset corePackages
  pure pset

flagAssignments :: Map PackageName FlagAssignment
flagAssignments = fromList (readFlagAssignents flagList)

forcedExectables :: Set PackageName
forcedExectables = forcedExectableNames

corePackages :: PackageSet
corePackages = ghcCorePackages
