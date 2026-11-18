module Oracle
  ( -- * Access to Hackage
    addHackageCache, addCabalFileCache
  , parseCabalFile, hasLibrary, packageRevision
    -- * Configuration
  , addFlagAssignmentOracle
  )
  where

import Oracle.Flags
import Oracle.Hackage
