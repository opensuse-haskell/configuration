{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config where

import Control.Monad
import Control.Exception
import Data.Set as Set
import Data.Map as Map
import Distribution.Verbosity
import Data.Maybe
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Text
import GHC.Generics ( Generic )
import System.IO.Error
import Orphans ()
import Distribution.Version
import Distribution.PackageDescription.Parse

import ParseUtils

newtype BuildName = BuildName { unBuildName :: String }
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

newtype PackageSetId = PackageSetId { unPackageSetId :: String }
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

newtype Revision = Revision { unRevision :: Int }
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

data BuildDescription = BuildDescription
  { pid :: PackageIdentifier
  , prv :: Revision
  , hasLib :: Bool
  , hasExe :: [FilePath]
  , forcedExe :: Bool
  , flags :: String
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable BuildDescription
instance NFData BuildDescription
instance Binary BuildDescription

instance Package BuildDescription where
  packageId = pid

-- data PackageSet = PackageSet
--   { packages :: Map BuildName BuildDescription
--   , compiler :: CompilerId
--   }
--   deriving (Show, Eq, Ord)

-- type Config = Map PackageSetId PackageSet

-- readConfig :: FilePath -> Action Config
-- readConfig p = do
--   psetTargets <- fmap PackageSetId <$> getDirectoryDirs p
--   psets <- mapM (readPackageSetConfig p) psetTargets
--   return $ Map.fromList (zip psetTargets psets)

-- readPackageSetConfig :: FilePath -> PackageSetId -> Action PackageSet
-- readPackageSetConfig p (PackageSetId psid) = do
--   bannedPackagesList <- readPackageNameList (p </> psid </> "banned-packages.txt")
--   extraPackagesList <- readConstraintList (p </> psid </> "extra-packages.txt")
--   forcedExecutableList <- readPackageNameList (p </> psid </> "forced-executable-packages.txt")
--   flagAssignment <- undefined
--   compilerIdBuf <- readFile' (p </> psid </> "compiler.txt")
--   stackagePackageList <- undefined
--   cid <- parseText "compiler id" compilerIdBuf

--   vs <- forP extraPackagesList undefined -- resolveConstraint

--   let stackagePackages =  Prelude.filter (\(PackageIdentifier pn _) -> pn `notElem` bannedPackagesList) stackagePackageList
--                        ++ [ PackageIdentifier pn v  | (Dependency pn _, v) <- zip extraPackagesList vs ]

--   need [ p </> ".." </>  cabalFilePath pId | pId <- stackagePackageList ]
--   pkgs <- forP stackagePackages $ \pId@(PackageIdentifier pn@(PackageName n) _) -> do
--     cabal <- liftIO $ readPackageDescription silent (p </> ".." </> cabalFilePath pId)
--     let bd = BuildDescription
--                { pid = pId
--                , prv = Revision (maybe 0 read (Prelude.lookup "x-revision" (customFieldsPD (packageDescription cabal))))
--                , hasLib = isJust (library (packageDescription cabal))
--                , hasExe = Prelude.map exeName (executables (packageDescription cabal))
--                , forcedExe = pn `elem` forcedExecutableList
--                , flags = fromMaybe "" (Prelude.lookup n flagAssignment)
--                }
--         bn = BuildName $ (if hasLib bd && not (forcedExe bd) then "ghc-" else "") ++ n
--     return (bn,bd)
--   return $ PackageSet (Map.fromList pkgs) cid

-- readConfigFile :: FilePath -> Action [String]
-- readConfigFile p = do
--   buf <- readFileLines p
--   return [ l | l@(c:_) <- buf, c /= '#' ]

-- readConstraintList :: FilePath -> Action [Dependency]
-- readConstraintList p = readConfigFile p >>= \x -> liftIO $  fileErrorContext p (mapM (parseText "constraint") x)

-- readPackageNameList :: FilePath -> Action [PackageName]
-- readPackageNameList p = readConfigFile p >>= \x -> liftIO $ fileErrorContext p (mapM (parseText "package name") x)

-- fileErrorContext :: FilePath -> IO a -> IO a
-- fileErrorContext p = modifyIOError (\e -> annotateIOError e "" Nothing (Just p))

-- main :: IO ()
-- main = do
--   let buildDir = "/tmp/_build"
--   shakeArgs shakeOptions { shakeFiles=buildDir, shakeProgress=progressSimple } $
--     action $ do
--       config <- readConfig "../../config"
--       putNormal (show config)
