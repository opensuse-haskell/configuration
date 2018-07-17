{-# LANGUAGE TypeFamilies #-}

module Oracle.Hackage where

import Data.Maybe
import Development.Shake
import Development.Shake.FilePath
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Text
import Distribution.Verbosity
import Distribution.Version

import Orphans ()
import ParseUtils

type instance RuleResult Dependency = Version

-- | Resolve a Hackage 'Dependency' into the latest 'Version' that satisfies
-- the constraint or report an error via 'fail'. The first `FilePath` argument
-- to this function is supposed to point to a checked out copy of the
-- <https://github.com/commercialhaskell/all-cabal-files> repository.

addConstraintResolverOracle :: FilePath -> Rules (Dependency -> Action Version)
addConstraintResolverOracle hackageDir = addOracle $ \c@(Dependency name vrange) -> do
  vs <- getDirectoryDirs (hackageDir </> unPackageName name) >>= mapM (parseText "version number")
  case filter (`withinRange` vrange) vs of
    []  -> fail ("cannot resolve " ++ show (display c) ++ " in Hackage")
    vs' -> return (maximum vs')

-- | Helper function that maps a given 'PackageIdentifier' into the 'FilePath'
-- of the corresponding Cabal file. The first function argument gives the
-- location of a checked out copy of the
-- <https://github.com/commercialhaskell/all-cabal-files> repository.

cabalFilePath :: FilePath -> PackageIdentifier -> FilePath
cabalFilePath hackageDir (PackageIdentifier n v) =
  hackageDir </> unPackageName n </> display v </> unPackageName n <.> "cabal"

-- | Cached access to parsed ADT that represent the Cabal files registered in
-- Hackage.

hackageDB :: FilePath -> Rules (PackageIdentifier -> Action GenericPackageDescription)
hackageDB hackageDir = newCache $ \pid -> do
  let p = cabalFilePath hackageDir pid
  need [p]
  liftIO (readGenericPackageDescription silent p)

-- | Extract the @x-revision@ header added by Hackage.

packageRevision :: GenericPackageDescription -> Int
packageRevision = maybe 0 read . lookup "x-revision" . customFieldsPD . packageDescription

-- | Does the given Cabal build define a library component?

hasLibrary :: GenericPackageDescription -> Bool
hasLibrary gpd = isJust (library (packageDescription gpd)) || isJust (condLibrary gpd)
