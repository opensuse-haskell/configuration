{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Development.Shake.Classes

newtype BuildName = BuildName { unBuildName :: String }
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

newtype PackageSetId = PackageSetId { unPackageSetId :: String }
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)
