{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Orphan instances live in a separate module so that we can selectively
-- disable the compiler warning.

module Orphans ( ) where

import Development.Shake.Classes
import Distribution.Hackage.DB

instance Hashable PackageIdentifier
instance Hashable PackageName
