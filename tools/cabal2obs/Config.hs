{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Config ( getPackageSet, packageSets ) where

import Config.Ghc84x
import Config.LTS11
import Config.Nightly
import Orphans ()
import Types

import Data.Map.Strict ( Map, findWithDefault )

packageSets :: Map PackageSetId PackageSetConfig
packageSets = [ ("ghc-8.4.x", ghc84x)
              , ("lts-11",    lts11)
              , ("nightly",   nightly)
              ]

getPackageSet :: PackageSetId -> PackageSetConfig
getPackageSet psid = findWithDefault err psid packageSets
  where
    err = error ("unknown package set " ++ show (unPackageSetId psid))
