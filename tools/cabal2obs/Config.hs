{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Config ( getPackageSet, knownPackageSets ) where

import Config.Ghc84x
import Config.LTS11
import Config.Nightly
import Orphans ()
import Types

import Data.Map.Strict ( Map, findWithDefault, keysSet )
import Data.Set ( Set )

packageSets :: Map PackageSetId PackageSetConfig
packageSets = [ ("ghc-8.4.x", ghc84x)
              , ("lts-11",    lts11)
              , ("nightly",   nightly)
              ]

knownPackageSets :: Set PackageSetId
knownPackageSets = keysSet packageSets

getPackageSet :: Monad m => PackageSetId -> m PackageSetConfig
getPackageSet psid = return (findWithDefault err psid packageSets)
  where
    err = error ("unknown package set " ++ show (unPackageSetId psid))
