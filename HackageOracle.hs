module HackageOracle where

import Development.Shake
import Development.Shake.FilePath
import Distribution.Text
import Distribution.Package
import Distribution.Version

import ParseUtils

resolveConstraint :: Dependency -> Action Version
resolveConstraint c@(Dependency (PackageName name) vrange) = do
  vs <- getDirectoryDirs ("../../hackage" </> name) >>= mapM (parseText "version number")
  case filter (`withinRange` vrange) vs of
    []  -> fail ("cannot resolve " ++ show (display c) ++ " in Hackage")
    vs' -> return (maximum vs')

-- main :: IO ()
-- main = do
--   let buildDir = "/tmp/_build"
--   shakeArgs shakeOptions { shakeFiles=buildDir, shakeProgress=progressSimple } $ do
--     action $ do
--       v <- resolveConstraint (Dependency (PackageName "cabal2nix") anyVersion)
--       putNormal (show v)
