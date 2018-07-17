{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Orphan instances live in a separate module so that we can selectively
-- disable the compiler warning.

module Orphans ( ) where

import Data.Maybe
import Data.Map.Strict as Map
import Data.Set as Set
import Data.String
import Development.Shake.Classes
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Text
import Distribution.Utils.ShortText
import Distribution.Version

instance Hashable PackageIdentifier
instance Hashable PackageName
instance Hashable CompilerFlavor
instance Hashable CompilerId
instance Hashable FlagName
instance Hashable Dependency
instance Hashable VersionRange
instance Hashable Version
instance Hashable ShortText

instance Hashable FlagAssignment where
  hashWithSalt salt = hashWithSalt salt . unFlagAssignment

instance Hashable v => Hashable (Set v) where
  hashWithSalt s = hashWithSalt s . Set.toAscList

instance (Hashable k, Hashable v) => Hashable (Map k v) where
  hashWithSalt s = hashWithSalt s . Map.toAscList

instance IsString Dependency where
  fromString = parseText "Dependency"

instance IsString VersionRange where
  fromString = parseText "VersionRange"

instance IsString Version where
  fromString = parseText "Version"

instance IsString CompilerId where
  fromString = parseText "CompilerId"

instance IsString PackageIdentifier where
  fromString = parseText "PackageIdentifier"

instance IsString (PackageName, Version) where
  fromString x = (n, v) where PackageIdentifier n v = fromString x

instance IsString (PackageName, VersionRange) where
  fromString x = (n, vr) where Dependency n vr = fromString x

parseText :: (Text a) => String -> String -> a
parseText errM buf = fromMaybe (error ("invalid " ++ errM ++ ": " ++ show buf)) (simpleParse buf)
