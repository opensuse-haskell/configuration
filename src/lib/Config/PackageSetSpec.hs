{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config.PackageSetSpec ( PackageSetSpec(..) ) where

import MyCabal ( CompilerId, FlagAssignment, PackageName )
import Types

import Data.Map.Strict ( Map )
import Data.Set ( Set )
import Development.Shake.Classes
import GHC.Generics ( Generic )

-- | Everything that defines a package set.

data PackageSetSpec = PackageSetSpec
  { packageSetName        :: FilePath                       -- ^ The name of the package set, e.g. @ghc-9.14.x@, derived from the compiler.
  , compiler              :: CompilerId                     -- ^ The compiler we build with.
  , corePackages          :: PackageSet                     -- ^ The packages bundled with the compiler.
  , packageSet            :: PackageSet                     -- ^ The packages we generate builds for.
  , targetPackages        :: ConstraintSet                  -- ^ The packages the package set is resolved for.
  , allowNewer            :: Map PackageName PackageNameSet -- ^ Version bounds we ignore when resolving the package set.
  , flags                 :: Map PackageName FlagAssignment -- ^ The flags we configure for packages.
  , forcedExecutables     :: PackageNameSet                 -- ^ Packages we build as executables even though they have a library.
  , pkgConfigDependencies :: Set NixpkgsAttribute           -- ^ System libraries that packages look up via pkg-config.
  }
  deriving (Show, Eq, Generic, Hashable, Binary, NFData)
