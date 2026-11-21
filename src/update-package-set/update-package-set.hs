-- | update-package-set: resolve the target packages of our package set and
-- print the resulting package set.

module Main ( main ) where

import Config
import UpdatePackageSet

import Text.PrettyPrint ( render )

main :: IO ()
main = readPackageSetSpec configFile >>= cabalInstallResolver >>= putStrLn . render . ppPackageSet
  where
    configFile = "conf/default-package-set.yaml"
