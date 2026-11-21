{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- | The configuration of our package set. This is the only module of the
-- configuration that is visible outside of the config-data library.

module Config
  ( -- * Specification of a package set
    PackageSetSpec(..), packageSetSpec
    -- * Tools
  , getPackageSetName, ppPackageSet, ppConstraintSet, updateTargetPackages
  ) where

import qualified Config.Compiler
import qualified Config.Flags
import qualified Config.ForcedExecutables
import qualified Config.PackageSet
import qualified Config.PkgConfig
import Config.Tools
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

-- | Our package set. Fails if the configuration is inconsistent.

packageSetSpec :: MonadFail m => m PackageSetSpec
packageSetSpec = do
  compiler              <- Config.Compiler.ghcCompiler
  packageSetName        <- getPackageSetName compiler
  corePackages          <- Config.Compiler.ghcCorePackages
  packageSet            <- Config.PackageSet.packageSet
  targetPackages        <- Config.PackageSet.targetPackages
  allowNewer            <- Config.PackageSet.allowNewer
  flags                 <- Config.Flags.buildFlags
  forcedExecutables     <- Config.ForcedExecutables.forcedExectableNames
  pkgConfigDependencies <- Config.PkgConfig.pkgConfigDependencies
  pure PackageSetSpec{..}
