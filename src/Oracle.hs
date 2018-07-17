module Oracle
  ( -- * Access to Hackage
    addConstraintResolverOracle
  , cabalFilePath
  , packageRevision
  , hackageDB
  , hasLibrary

  --   -- * Access to configured package lists
  -- , getPackageList, GetPackageList(..)
  -- , getForcedExes, GetForcedExes(..)
  -- , getCompiler, GetCompiler(..)
  -- , getFlagAssignments, GetFlagAssignments(..)
  )
  where

-- import Oracle.Compiler
-- import Oracle.FlagAssignment
-- import Oracle.ForcedExecutables
import Oracle.Hackage
-- import Oracle.PackageList
