{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Oracle.Compiler where

import Config
import Orphans ( )
import Types

import Development.Shake
import Development.Shake.Classes
import Distribution.Compiler

newtype GetCompiler = GetCompiler PackageSetId
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

type instance RuleResult GetCompiler = CompilerId

getCompiler :: Rules (GetCompiler -> Action CompilerId)
getCompiler = addOracle $ \(GetCompiler psid) -> return (compiler (getConfig psid))
