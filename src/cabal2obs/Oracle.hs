module Oracle
  ( -- * Access to Hackage
    addHackageCache, addCabalFileCache
  , parseCabalFile, hasLibrary, packageRevision
    -- * Configuration
  , PackageSetSpec, addPackageSetSpecOracle
  , addPackageSetOracle, addAvailablePackagesOracle
  , addFlagAssignmentOracle, addForcedExecutableOracle, addCompilerOracle, readPackageSetName
  )
  where

import Oracle.Hackage
import Oracle.PackageSetSpec
