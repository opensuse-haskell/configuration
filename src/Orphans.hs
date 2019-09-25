{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Orphan instances live in a separate module so that we can selectively
-- disable the compiler warning.

module Orphans ( ) where

import OpenSuse.Prelude

import Data.Map.Strict as Map
import Data.Set as Set
import Development.Shake.Classes
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Parsec
import qualified Distribution.Pretty as Cabal
import Distribution.Types.PackageVersionConstraint
import Distribution.Types.UnqualComponentName
import Distribution.Utils.ShortText
import Distribution.Version

instance Hashable UnqualComponentName
instance Hashable LibraryName
instance Hashable PackageIdentifier
instance Hashable PackageName
instance Hashable CompilerFlavor
instance Hashable CompilerId
instance Hashable FlagName
instance Hashable Dependency
instance Hashable VersionRange
instance Hashable Version
instance Hashable ShortText
instance Hashable PackageVersionConstraint

instance Hashable FlagAssignment where
  hashWithSalt salt = hashWithSalt salt . unFlagAssignment

instance Hashable v => Hashable (Set v) where
  hashWithSalt s = hashWithSalt s . Set.toAscList

instance (Hashable k, Hashable v) => Hashable (Map k v) where
  hashWithSalt s = hashWithSalt s . Map.toAscList

instance Pretty Version where
  pPrint = Cabal.pretty

instance Pretty PackageIdentifier where
  pPrint = Cabal.pretty

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
  fromString x = (n, vr) where Dependency n vr _ = fromString x

parseText :: (Parsec a) => String -> String -> a
parseText errM buf = fromMaybe (error ("invalid " ++ errM ++ ": " ++ show buf)) (simpleParsec buf)
