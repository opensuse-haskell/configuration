{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Oracle.Compiler ( addCompilerOracle ) where

import Config ( PackageSetSpec(compiler), packageSetSpec )
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
  getCompiler <- addOracle $ \(Compiler ()) -> pure (compiler packageSetSpec)
  pure (getCompiler (Compiler ()))
