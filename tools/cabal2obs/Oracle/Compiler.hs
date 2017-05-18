{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Oracle.Compiler where

import Config
import Orphans ( )
import Types

import Development.Shake
import Development.Shake.Classes
import Distribution.Compiler

newtype GetCompiler = GetCompiler PackageSetId
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

getCompiler :: Rules (GetCompiler -> Action CompilerId)
getCompiler = addOracle $ \(GetCompiler psid) -> return (compiler (getConfig psid))
