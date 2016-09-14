{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Orphan instances live in a separate module so that we can selectively
-- disable the compiler warning.

module Orphans ( ) where

-- import Data.Map as Map
-- import Data.Set as Set
import Development.Shake.Classes
import Distribution.Compiler
-- import Distribution.License
-- import Distribution.ModuleName
import Distribution.Package
import Distribution.PackageDescription
-- import Distribution.System
import Distribution.Version
-- import Language.Haskell.Extension

instance Hashable PackageIdentifier
instance Hashable PackageName
-- instance (Hashable v, Hashable c, Hashable a) => Hashable (CondTree v c a)
-- instance (Hashable c) => Hashable (Condition c)
-- instance Hashable GenericPackageDescription
-- instance Hashable TestSuite
-- instance Hashable TestSuiteInterface
-- instance Hashable PackageDescription
-- instance Hashable SourceRepo
-- instance Hashable RepoType
-- instance Hashable RepoKind
-- instance Hashable SetupBuildInfo
-- instance Hashable BuildInfo
instance Hashable Dependency
instance Hashable VersionRange
-- instance Hashable Library
-- instance Hashable ModuleReexport
-- instance Hashable ModuleName
-- instance Hashable ModuleRenaming
-- instance Hashable Extension
-- instance Hashable KnownExtension
-- instance Hashable Language
instance Hashable CompilerFlavor
instance Hashable CompilerId
-- instance Hashable Flag
instance Hashable FlagName
-- instance Hashable TestType
-- instance Hashable Executable
-- instance Hashable ConfVar
-- instance Hashable Benchmark
-- instance Hashable BenchmarkInterface
-- instance Hashable BenchmarkType
-- instance Hashable BuildType
-- instance Hashable License
-- instance Hashable Arch
-- instance Hashable OS

-- instance (NFData v, NFData c, NFData a) => NFData (CondTree v c a)
-- instance (NFData c) => NFData (Condition c)
-- instance NFData GenericPackageDescription
-- instance NFData TestSuite
-- instance NFData TestSuiteInterface
-- instance NFData PackageDescription
-- instance NFData SourceRepo
-- instance NFData RepoType
-- instance NFData RepoKind
-- instance NFData SetupBuildInfo
-- instance NFData BuildInfo
instance NFData Dependency
instance NFData VersionRange
-- instance NFData Library
-- instance NFData ModuleReexport
-- instance NFData ModuleName
-- instance NFData ModuleRenaming
-- instance NFData Extension
-- instance NFData KnownExtension
-- instance NFData Language
instance NFData CompilerFlavor
instance NFData CompilerId
-- instance NFData Flag
instance NFData FlagName
-- instance NFData TestType
-- instance NFData Executable
-- instance NFData ConfVar
-- instance NFData Benchmark
-- instance NFData BenchmarkInterface
-- instance NFData BenchmarkType
-- instance NFData BuildType
-- instance NFData License
-- instance NFData Arch
-- instance NFData OS

-- instance (Hashable a, Hashable b) => Hashable (Map a b) where
--   hashWithSalt = Map.foldlWithKey' (\s k v -> hashWithSalt (hashWithSalt s k) v)

-- instance (Hashable a) => Hashable (Set a) where
--   hashWithSalt = Set.foldl' hashWithSalt
