module Oracle
  ( -- * Access to Hackage
    addHackageCache, addCabalFileCache
  , parseCabalFile, hasLibrary, packageRevision
    -- * Configuration
  , addFlagAssignmentOracle, addAvailablePackagesOracle, addCompilerOracle
  )
  where

import Oracle.Compiler
import Oracle.Flags
import Oracle.Hackage
import Oracle.PackageSet
