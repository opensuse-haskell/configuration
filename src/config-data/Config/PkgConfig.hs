{-# LANGUAGE OverloadedLists #-}

module Config.PkgConfig ( pkgConfigDependencies ) where

import Types

import Data.Set ( Set )

-- | System libraries (as nixpkgs attribute names) that packages in our set
-- look up via pkg-config, depending on the flags we choose.

pkgConfigDependencies :: MonadFail m => m (Set NixpkgsAttribute)
pkgConfigDependencies = pure [ "libyaml", "sqlite", "icu", "pango", "cairo", "glib" ]
