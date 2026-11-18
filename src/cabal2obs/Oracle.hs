module Oracle
  ( -- * Access to Hackage
    addHackageCache, addCabalFileCache
  , parseCabalFile, hasLibrary, packageRevision
    -- * Configuration
  , addFlagAssignmentOracle, addAvailablePackagesOracle
  )
  where

import Oracle.Flags
import Oracle.Hackage
import Oracle.PackageSet
