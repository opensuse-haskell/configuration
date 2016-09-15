{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Oracle.ForcedExecutables where

import Orphans ( )
import ParseUtils
import Types

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Classes
import Distribution.Package

newtype GetForcedExes = GetForcedExes PackageSetId
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

getForcedExes :: FilePath -> Rules (GetForcedExes -> Action [PackageName])
getForcedExes configDir = addOracle $ \(GetForcedExes (PackageSetId psid)) ->
  readPackageNameList (configDir </> psid </> "forced-executable-packages.txt")
