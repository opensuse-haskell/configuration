module Oracle
  ( -- * Access to Hackage
    addHackageCache, addCabalFileCache
  , parseCabalFile, hasLibrary, packageRevision
  )
  where

import Oracle.Hackage
