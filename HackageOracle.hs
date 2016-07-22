{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Distribution.Hackage.DB as DB
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Classes
import Orphans ()
import GHC.Generics ( Generic )

newtype QueryHackage = QueryHackage PackageName
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

getHackageDB :: Rules (() -> Action Hackage)
getHackageDB = newCache $ \() -> do
  p <- liftIO hackagePath
  need [p]
  liftIO (readHackage' p)

main :: IO ()
main = do
  let buildDir = "/tmp/_build"
  shakeArgs shakeOptions { shakeFiles=buildDir, shakeProgress=progressSimple } $ do
    -- Load the configuration
    getHackage <- newCache $ \() -> do
      p <- liftIO hackagePath
      need [p]
      liftIO (readHackage' p)

    findPackageVersions <- addOracle $ \(PackageName n) -> do
      DB.lookup n <$> getHackage ()


    -- Define the rules.
    "latest-*.txt" %> \out -> do
      let pkgname = drop 7 (takeBaseName out)
      vs <- findPackageVersions (PackageName pkgname)
      putNormal $ "building: " ++ out ++ " from " ++ show (pkgname,vs)

    -- Go.
    return ()
