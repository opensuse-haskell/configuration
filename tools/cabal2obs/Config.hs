{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Config where

import Orphans ()
import Types

import Data.Maybe
import Data.Map.Strict as Map

packageSets :: Map PackageSetId PackageSetConfig
packageSets = [ ("ghc-8.4.x", ghc84x)
              ]

getPackageSet :: PackageSetId -> PackageSetConfig
getPackageSet psid = findWithDefault err psid packageSets
  where
    err = error ("unknown package set " ++ show (unPackageSetId psid))

ghc84x :: PackageSetConfig
ghc84x = PackageSetConfig
  { compiler         = "ghc-8.4.2"
  , targetPackages   = [ "cabal-install >2.2" ]
  , packageSet       = [ "base16-bytestring-0.1.1.6"
                       , "base64-bytestring-1.0.0.1"
                       , "cryptohash-sha256-0.11.101.0"
                       , "echo-0.1.3"
                       , "ed25519-0.0.5.0"
                       , "hashable-1.2.7.0"
                       , "network-2.6.3.5"
                       , "network-uri-2.6.1.0"
                       , "random-1.1"
                       , "tar-0.5.1.0"
                       , "zlib-0.6.2"
                       , "resolv-0.1.1.1"
                       , "async-2.2.1"
                       , "HTTP-4000.3.11"
                       , "edit-distance-0.2.2.1"
                       , "hackage-security-0.5.3.0"
                       , "cabal-install-2.2.0.0"
                       ]
  , flagAssignments  = []
  , forcedExectables = [ "cabal-install" ]
  }
