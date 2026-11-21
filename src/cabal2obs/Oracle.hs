module Oracle
  ( -- * Access to Hackage
    addHackageCache, addCabalFileCache
  , parseCabalFile, hasLibrary, packageRevision
    -- * Configuration
  , PackageSetSpec(..), readPackageSetSpec, addPackageSetSpecOracle
  , addPackageSetOracle, addAvailablePackagesOracle
  , addFlagAssignmentOracle, addForcedExecutableOracle, addCompilerOracle
  )
  where

import Oracle.Hackage
import Oracle.PackageSetSpec
