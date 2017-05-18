{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Oracle.FlagAssignment where

import Orphans ()
import Types
import Config

import Development.Shake
import Development.Shake.Classes
import Distribution.Package
import Distribution.PackageDescription

newtype GetFlagAssignments = GetFlagAssignments PackageSetId
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

getFlagAssignments :: Rules (GetFlagAssignments -> Action [(PackageName,FlagAssignment)])
getFlagAssignments = addOracle $ \(GetFlagAssignments psid) ->
  return (flagAssignments (getConfig psid) )
