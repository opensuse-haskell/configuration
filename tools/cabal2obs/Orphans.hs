{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Orphan instances live in a separate module so that we can selectively
-- disable the compiler warning.

module Orphans ( ) where

import Data.Maybe
import Data.String
import Development.Shake.Classes
import Distribution.Text
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import Distribution.Utils.ShortText

instance Hashable PackageIdentifier
instance Hashable PackageName
instance Hashable CompilerFlavor
instance Hashable CompilerId
instance Hashable FlagName
instance Hashable Dependency
instance Hashable VersionRange
instance Hashable Version
instance Hashable ShortText

instance NFData CompilerFlavor
instance NFData CompilerId
instance NFData FlagName

instance IsString Dependency where
  fromString = parseText "Dependency"

instance IsString CompilerId where
  fromString = parseText "CompilerId"

parseText :: (Text a) => String -> String -> a
parseText errM buf = fromMaybe (error ("invalid " ++ errM ++ ": " ++ show buf)) (simpleParse buf)
