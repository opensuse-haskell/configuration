module Oracle
  ( -- * Access to Hackage
    addHackageCache, addCabalFileCache, addConstraintResolverOracle
  , parseCabalFile, hasLibrary, packageRevision
  )
  where

import Oracle.Hackage
