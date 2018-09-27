{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Config ( knownPackageSets, addConfigOracle ) where

import Config.Ghc86x
import Config.LTS12
import Orphans ()
import Types

import Data.Map.Strict ( Map, findWithDefault, keysSet )
import Data.Set ( Set )
import Development.Shake

type instance RuleResult PackageSetId = PackageSetConfig

addConfigOracle :: Rules (PackageSetId -> Action PackageSetConfig)
addConfigOracle = addOracle getPackageSet

packageSets :: Map PackageSetId (Action PackageSetConfig)
packageSets = [ ("ghc-8.6.x", ghc86x)
              , ("lts-12",    lts12)
              ]

knownPackageSets :: Set PackageSetId
knownPackageSets = keysSet packageSets

getPackageSet :: PackageSetId -> Action PackageSetConfig
getPackageSet psid = findWithDefault err psid packageSets
  where
    err = fail ("unknown package set " ++ show (unPackageSetId psid))
