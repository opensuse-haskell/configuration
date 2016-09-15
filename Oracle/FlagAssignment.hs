{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Oracle.FlagAssignment where

import Orphans ()
import ParseUtils
import Types

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Classes

newtype GetFlagAssignments = GetFlagAssignments PackageSetId
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

-- TODO: Use FlagAssignment type.

getFlagAssignments :: FilePath -> Rules (GetFlagAssignments -> Action [(String,String)])
getFlagAssignments configDir = addOracle $ \(GetFlagAssignments (PackageSetId psid)) ->
  readFlagAssignents (configDir </> psid </> "flag-assignment.txt")

readFlagAssignents :: FilePath -> Action [(String,String)]
readFlagAssignents p = fmap (fmap (unwords . words . tail) . break (==':')) <$> readConfigFile p
