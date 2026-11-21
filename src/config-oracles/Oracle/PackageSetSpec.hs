{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Shake oracles that give access to the specification of our package set.
--
-- 'addPackageSetSpecOracle' tracks the entire specification. All other
-- oracles are built on top of it: each one queries the specification and
-- returns only the part of it that its users need. When the configuration
-- changes, these oracles are re-run, but Shake re-builds the rules that query
-- one of them only if its result has changed. Rules should therefore query
-- the most specific oracle that gives them what they need.

module Oracle.PackageSetSpec
  ( PackageSetSpec(..)
  , addPackageSetSpecOracle
  , addPackageSetOracle, addAvailablePackagesOracle
  , addFlagAssignmentOracle, addForcedExecutableOracle, addCompilerOracle
  , readPackageSetName
  ) where

import Config ( PackageSetSpec(..), packageSetSpec )
import MyCabal
import Types

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Development.Shake
import Development.Shake.Classes

-- | Key for the oracle that tracks the entire specification of our package
-- set.
newtype ThePackageSetSpec = ThePackageSetSpec ()
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult ThePackageSetSpec = PackageSetSpec

-- | Register an oracle that gives the entire specification of our package
-- set. Rules that query it are re-built whenever any part of the
-- configuration changes, so rules that care only about some aspects of it
-- should query one of the more specific oracles below instead. Those take the
-- action this function returns as their argument.

addPackageSetSpecOracle :: Rules (Action PackageSetSpec)
addPackageSetSpecOracle = do
  getPackageSetSpec <- addOracle $ \(ThePackageSetSpec ()) -> packageSetSpec
  pure (getPackageSetSpec (ThePackageSetSpec ()))

-- | Key for the oracle that tracks our package set.
newtype ThePackageSet = ThePackageSet ()
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult ThePackageSet = PackageSet

-- | Register an oracle that gives the package set we generate builds for.
-- Rules that query it are re-built whenever the package set changes in any
-- way, so rules that care only about some aspects of it should query a more
-- specific oracle instead, e.g. 'addAvailablePackagesOracle'.

addPackageSetOracle :: Action PackageSetSpec -> Rules (Action PackageSet)
addPackageSetOracle getSpec = do
  getPackageSet <- addOracle $ \(ThePackageSet ()) -> packageSet <$> getSpec
  pure (getPackageSet (ThePackageSet ()))

-- | Key for the oracle that tracks which packages are available to satisfy
-- dependencies.
newtype AvailablePackages = AvailablePackages ()
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult AvailablePackages = PackageNameSet

-- | Register an oracle that gives the names of all packages that are
-- available to satisfy dependencies, i.e. those in our package set plus the
-- compiler's core packages. Rules that query it are re-built when a package is
-- added to or removed from that set. Changes of package versions don't affect
-- the result.

addAvailablePackagesOracle :: Action PackageSetSpec -> Rules (Action PackageNameSet)
addAvailablePackagesOracle getSpec = do
  getAvailablePackages <- addOracle $ \(AvailablePackages ()) ->
    (\spec -> Map.keysSet (packageSet spec `Map.union` corePackages spec)) <$> getSpec
  pure (getAvailablePackages (AvailablePackages ()))

-- | Key for the oracle that tracks the flag assignment of a package.
newtype FlagsFor = FlagsFor PackageName
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult FlagsFor = FlagAssignment

-- | Register an oracle that gives the flag assignment of a package from our
-- configuration, defaulting to no assignments. Rules that query it are
-- re-built when a package's flags change in the configuration.

addFlagAssignmentOracle :: Action PackageSetSpec -> Rules (PackageName -> Action FlagAssignment)
addFlagAssignmentOracle getSpec = do
  getFlags <- addOracle $ \(FlagsFor pn) -> Map.findWithDefault mempty pn . flags <$> getSpec
  pure (getFlags . FlagsFor)

-- | Key for the oracle that tracks whether a package is forced to be packaged
-- as an executable, even though it has a library.
newtype IsForcedExecutable = IsForcedExecutable PackageName
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult IsForcedExecutable = Bool

-- | Register an oracle that tells whether a package is listed in our
-- configuration as a forced executable. Rules that query it are re-built when
-- a package is added to or removed from that list.

addForcedExecutableOracle :: Action PackageSetSpec -> Rules (PackageName -> Action Bool)
addForcedExecutableOracle getSpec = do
  isForced <- addOracle $ \(IsForcedExecutable pn) -> Set.member pn . forcedExecutables <$> getSpec
  pure (isForced . IsForcedExecutable)

-- | Key for the oracle that tracks the compiler we generate builds for.
newtype Compiler = Compiler ()
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult Compiler = CompilerId

-- | Register an oracle that gives the compiler we generate builds for. Rules
-- that query it are re-built when the compiler changes in the configuration.

addCompilerOracle :: Action PackageSetSpec -> Rules (Action CompilerId)
addCompilerOracle getSpec = do
  getCompiler <- addOracle $ \(Compiler ()) -> compiler <$> getSpec
  pure (getCompiler (Compiler ()))

-- | The name of the package set we generate builds for, e.g. @ghc-9.14.x@.
-- This is no oracle: the name is part of every path the build system creates,
-- so if it changes, Shake re-builds everything anyway.

readPackageSetName :: MonadFail m => m FilePath
readPackageSetName = packageSetName <$> packageSetSpec
