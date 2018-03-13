module Config where

import Config.LTS8
import Config.LTS9
import Config.LTS10
import Config.Nightly
import Types

knownPackageSets :: [PackageSetId]
knownPackageSets = [PackageSetId "lts-10", PackageSetId "nightly"]

getConfig :: PackageSetId -> PackageSetConfig
getConfig (PackageSetId "lts-8")   = lts8
getConfig (PackageSetId "lts-9")   = lts9
getConfig (PackageSetId "lts-10")  = lts10
getConfig (PackageSetId "nightly") = nightly
getConfig psid                     = error $ "getConfig: unknown package set " ++ show psid

getConfigDirname :: PackageSetId -> String
getConfigDirname (PackageSetId ('l':'t':'s':'-':n)) = 'L':'T':'S':n
getConfigDirname (PackageSetId "nightly")           = "Nightly"
getConfigDirname psid                               = error $ "getConfigDirname: unknown package set " ++ show psid
