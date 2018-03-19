module Config where

import Config.LTS8
import Config.LTS9
import Config.LTS10
import Config.LTS11
import Config.Nightly
import Types

import Data.List

knownPackageSets :: [PackageSetId]
knownPackageSets = [PackageSetId "lts-11", PackageSetId "lts-next"]

getConfig :: PackageSetId -> PackageSetConfig
getConfig (PackageSetId "lts-8")    = lts8
getConfig (PackageSetId "lts-9")    = lts9
getConfig (PackageSetId "lts-10")   = lts10
getConfig (PackageSetId "lts-11")   = lts11
getConfig (PackageSetId "lts-next") = nightly
getConfig psid                      = error $ "getConfig: unknown package set " ++ show psid

getConfigDirname :: PackageSetId -> String
getConfigDirname (PackageSetId psid)
  | Just suff <- stripPrefix "lts-" psid = if suff == "next"
                                              then "Nightly"
                                              else "LTS" ++ suff
  | otherwise                            = error $ "getConfigDirname: unknown package set " ++ show psid
