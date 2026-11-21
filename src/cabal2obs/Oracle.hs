module Oracle
  ( -- * Access to Hackage
    addHackageCache, addCabalFileCache
  , parseCabalFile, hasLibrary, packageRevision
    -- * Configuration
  , addPackageSetOracle, addAvailablePackagesOracle
  , addFlagAssignmentOracle, addForcedExecutableOracle, addCompilerOracle, readPackageSetName
  )
  where

import Oracle.Compiler
import Oracle.Flags
import Oracle.ForcedExecutables
import Oracle.Hackage
import Oracle.PackageSet
