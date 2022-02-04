{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MyCabal
  ( Version, VersionRange, withinRange, noVersion, anyVersion, version0
  , CompilerId(..), Platform(..), OS(..), Arch(..), AbiTag(..), unknownCompilerInfo
  , prettyShow, display
  , PackageName, unPackageName, mkPackageName
  , PackageVersionConstraint(..)
  , PackageIdentifier(..)
  , Dependency(..), depPkgName
  , ComponentRequestedSpec(..)
  , FlagName, FlagAssignment, mkFlagAssignment, lowercase, mkFlagName
  , GenericPackageDescription(..), parseGenericPackageDescriptionMaybe, finalizePD
  , PackageDescription(..), Library(..), LibraryName(..)
  , UnqualComponentName, mkUnqualComponentName, unUnqualComponentName
  , License
    -- Parsec
  , Parsec(..), simpleParsec, ParsecParser, simpleParse, CharParsing, skipOptional
  , string, eof, fieldLineStreamFromString, runParsecParser, satisfy, char, spaces
  , sepBy, try, eitherParsec, parseText
  ) where

import OpenSuse.Prelude

import Development.Shake.Classes
import Distribution.Compat.CharParsing
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parsec
import Distribution.Parsec
import Distribution.Parsec.FieldLineStream
import qualified Distribution.Pretty as Cabal
import Distribution.SPDX.License
import Distribution.Simple.Utils ( lowercase )
import Distribution.System
import Distribution.Text
import Distribution.Types.ComponentRequestedSpec
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

instance Pretty License where
  pPrint = Cabal.pretty

instance Pretty Dependency where
  pPrint = Cabal.pretty

instance Pretty Version where
  pPrint = Cabal.pretty

instance Pretty PackageIdentifier where
  pPrint = Cabal.pretty

instance Pretty PackageName where
  pPrint = Cabal.pretty

instance IsString Dependency where
  fromString = parseText' "Dependency"

instance IsString VersionRange where
  fromString = parseText' "VersionRange"

instance IsString Version where
  fromString = parseText' "Version"

instance IsString CompilerId where
  fromString = parseText' "CompilerId"

instance IsString PackageIdentifier where
  fromString = parseText' "PackageIdentifier"

instance IsString (PackageName, Version) where
  fromString x = (n, v) where PackageIdentifier n v = fromString x

instance IsString (PackageName, VersionRange) where
  fromString x = (n, vr) where Dependency n vr _ = fromString x

parseText' :: (Parsec a) => String -> String -> a
parseText' errM buf = fromMaybe (error ("invalid " ++ errM ++ ": " ++ show buf)) (simpleParsec buf)

parseText :: (Parsec a, MonadFail m) => String -> String -> m a
parseText errM buf =
  maybe (fail ("invalid " ++ errM ++ ": " ++ show buf))
       pure 
        (simpleParsec buf)
