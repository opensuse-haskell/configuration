{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Oracle.Compiler ( addCompilerOracle, readPackageSetName ) where

import Config ( PackageSetSpec(compiler, packageSetName), packageSetSpec )
import MyCabal

import Development.Shake
import Development.Shake.Classes

-- | Key for the oracle that tracks the compiler we generate builds for.
newtype Compiler = Compiler ()
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult Compiler = CompilerId

-- | Register an oracle that gives the compiler we generate builds for. Rules
-- that query it are re-built when the compiler changes in the configuration.

addCompilerOracle :: Rules (Action CompilerId)
addCompilerOracle = do
  getCompiler <- addOracle $ \(Compiler ()) -> compiler <$> packageSetSpec
  pure (getCompiler (Compiler ()))

-- | The name of the package set we generate builds for, e.g. @ghc-9.14.x@.
-- This is no oracle: the name is part of every path the build system creates,
-- so if it changes, Shake re-builds everything anyway.

readPackageSetName :: MonadFail m => m FilePath
readPackageSetName = packageSetName <$> packageSetSpec
