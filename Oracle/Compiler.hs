{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Oracle.Compiler where

import Orphans ( )
import ParseUtils
import Types

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Classes
import Distribution.Compiler

newtype GetCompiler = GetCompiler PackageSetId
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

getCompiler :: FilePath -> Rules (GetCompiler -> Action CompilerId)
getCompiler configDir = addOracle $ \(GetCompiler (PackageSetId psid)) ->
  readFile' (configDir </> psid </> "compiler.txt") >>= parseText "compiler id"
