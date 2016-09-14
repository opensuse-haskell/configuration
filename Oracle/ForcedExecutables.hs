{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Oracle.ForcedExecutables where

import ParseUtils

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Classes
import Distribution.Package

import Orphans ( )
import Config ( PackageSetId(..) )

newtype GetForcedExes = GetForcedExes PackageSetId
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

getForcedExes :: FilePath -> Rules (GetForcedExes -> Action [PackageName])
getForcedExes configDir = addOracle $ \(GetForcedExes (PackageSetId psid)) ->
  readPackageNameList (configDir </> psid </> "forced-executable-packages.txt")
