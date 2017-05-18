module Config where

import Config.LTS8
import Types

knownPackageSets :: [PackageSetId]
knownPackageSets = [PackageSetId "lts-8"]

getConfig :: PackageSetId -> PackageSetConfig
getConfig (PackageSetId "lts-8") = lts8
getConfig psid                   = error $ "getConfig: unknown package set " ++ show psid
