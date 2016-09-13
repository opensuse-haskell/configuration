module HackageOracle where

import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Development.Shake
import Development.Shake.FilePath
import Distribution.Hackage.DB ( Hackage, hackagePath, readHackage', (!) )
import Distribution.Text
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
-- import Config

cacheHackageDB :: Rules (() -> Action Hackage)
cacheHackageDB = newCache $ \() -> do
  p <- liftIO hackagePath
  need [p]
  liftIO (readHackage' p)

resolveConstraint :: Action Hackage -> Dependency -> Action GenericPackageDescription
resolveConstraint getHackage c@(Dependency (PackageName name) vrange)
  | Just vset' <- Map.lookup name hackage
  , vset <- Set.filter (`withinRange` vrange) (Map.keysSet vset')
  , not (Set.null vset) = return (vset' ! Set.findMax vset)
  | otherwise           = fail ("cannot resolve " ++ show (display c) ++ " in Hackage")

-- hackageOracle :: Action Hackage -> Rules (Dependency -> Action BuildDescription)
-- hackageOracle getHackage = addOracle $ \x ->
--   fmap cabal2bd (getHackage >>= resolveConstraint x)

-- cabal2bd :: GenericPackageDescription -> BuildDescription
-- cabal2bd cabal = BuildDescription
--   { pid = packageId cabal
--   , prv = Revision (maybe 0 read (lookup "x-revision" (customFieldsPD pd)))
--   , hasLib = isJust (library pd)
--   , hasExe = map exeName (executables pd)
--   , forcedExe = False
--   , flags = []
--   }
--   where
--     pd = packageDescription cabal

-- main :: IO ()
-- main = do
--   let buildDir = "/tmp/_build"
--   shakeArgs shakeOptions { shakeFiles=buildDir, shakeProgress=progressSimple } $ do
--     -- Load the configuration
--     getHackage <- cacheHackageDB
--     resolveInHackage <- hackageOracle (getHackage ())

--     -- Define the rules.
--     "latest-*.txt" %> \out -> do
--       putNormal $ "building " ++ out
--       let pkgname = drop 7 (takeBaseName out)
--       bd <- resolveInHackage (Dependency (PackageName pkgname) anyVersion)
--       let v = packageVersion bd
--           Revision r = prv bd
--       writeFileChanged out (display v ++ "-r" ++ show r ++ "\n")

--     -- Go.
--     want ["latest-async.txt", "latest-linux-ptrace.txt"]
