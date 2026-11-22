{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Oracle.Hackage
  ( addHackageCache, addCabalFileCache, addPreferredVersionsOracle
  , parseCabalFile, hasLibrary, packageRevision
  ) where

import OpenSuse.Prelude
import MyCabal
import Types ( Constraint )

import Codec.Archive.Tar.Entry as Tar ( entryTime, entryContent, GenEntryContent(..) )
import Codec.Archive.Tar.Index ( TarIndex, TarIndexEntry(..) )
import qualified Codec.Archive.Tar.Index as Tar
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.List ( isPrefixOf )
import Data.Maybe
import Data.Time.Clock.POSIX
import Development.Shake
import Development.Shake.FilePath
import System.Directory
import System.IO

newtype Hackage = Hackage TarIndex

addHackageCache :: Rules (Action Hackage)
addHackageCache = (\f' -> f' ()) <$> newCache f
  where f () = do
          home <- liftIO (getAppUserDataDirectory "cabal")
          let idxFile = home </> "packages/hackage.haskell.org/01-index.tar.idx"
          need [idxFile]
          buf <- liftIO (BSS.readFile idxFile)
          case Tar.deserialise buf of
            Nothing -> fail (idxFile <> ": could not parse Hackage tarball index")
            Just (idx,rest) | BSS.null rest -> pure (Hackage idx)
                            | otherwise     -> fail (idxFile <> ": unexpected " <> show (BSS.length rest) <> " extra bytes at the end")

-- | Helper function that maps a given 'PackageIdentifier' into the 'FilePath'
-- of the corresponding Cabal file.

cabalFilePath :: PackageIdentifier -> FilePath
cabalFilePath (PackageIdentifier n v) =
  unPackageName n </> prettyShow v </> unPackageName n <.> "cabal"

-- | Cached access to parsed ADT that represent the Cabal files registered in
-- Hackage.

addCabalFileCache :: Action Hackage -> Rules (PackageIdentifier -> Action (UTCTime, ByteString))
addCabalFileCache hackage = newCache $ \pid -> do
  Hackage idx <- hackage
  off <- case Tar.lookup idx (cabalFilePath pid) of
    Nothing                 -> fail (prettyShow pid <> " is not a valid Hackage package")
    Just (TarFileEntry off) -> pure off
    Just (TarDir _)         -> fail ("looking up " <> prettyShow pid <> " in Hackage gives unexpected directory(!) result")
  home <- liftIO (getAppUserDataDirectory "cabal")
  let tarball = home </> "packages/hackage.haskell.org/01-index.tar"
  need [tarball]
  e <- liftIO $ withFile tarball ReadMode (`Tar.hReadEntry` off)
  case Tar.entryContent e of
    NormalFile buf _ -> pure (posixSecondsToUTCTime (realToFrac (entryTime e)), BSL.toStrict buf)
    x                -> fail (prettyShow pid <> ": unexpected entry type in tarball: " <> show x)

-- | Key for the oracle that tracks the preferred versions of a package.
newtype PreferredVersionsOf = PreferredVersionsOf PackageName
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult PreferredVersionsOf = Constraint

-- | Register an oracle that gives the preferred versions of a package, i.e.
-- the contents of the file @<name>/preferred-versions@ in the Hackage index.
-- Hackage appends a new entry whenever the preferences change; the tarball
-- index refers to the latest one. A package without such an entry has no
-- preference, which is the constraint @<name>@ on any version. Rules that
-- query the oracle are re-built when the preferred versions of the package
-- change.

addPreferredVersionsOracle :: Action Hackage -> Rules (PackageName -> Action Constraint)
addPreferredVersionsOracle hackage = do
  getPreferredVersions <- addOracle $ \(PreferredVersionsOf pn) -> do
    Hackage idx <- hackage
    let path = unPackageName pn </> "preferred-versions"
    case Tar.lookup idx path of
      Nothing                 -> parsePreferredVersions pn BSS.empty
      Just (TarDir _)         -> fail ("looking up " <> path <> " in Hackage gives unexpected directory(!) result")
      Just (TarFileEntry off) -> do
        home <- liftIO (getAppUserDataDirectory "cabal")
        let tarball = home </> "packages/hackage.haskell.org/01-index.tar"
        need [tarball]
        e <- liftIO $ withFile tarball ReadMode (`Tar.hReadEntry` off)
        case Tar.entryContent e of
          NormalFile buf _ -> parsePreferredVersions pn (BSL.toStrict buf)
          x                -> fail (path <> ": unexpected entry type in tarball: " <> show x)
  pure (getPreferredVersions . PreferredVersionsOf)

-- | Parse the contents of a @preferred-versions@ file of the given package.
-- Empty lines and comment lines starting with @--@ are ignored. The file must
-- contain at most one constraint, and that constraint must be on the given
-- package.

parsePreferredVersions :: MonadFail m => PackageName -> ByteString -> m Constraint
parsePreferredVersions pn buf =
  case filter relevant (BS8.unpack . BS8.strip <$> BS8.lines buf) of
    []  -> parseConstraint (unPackageName pn)
    [l] -> parseConstraint l
    ls  -> failure ("expected at most one constraint, but found " <> show (length ls))
  where
    relevant l = not (null l) && not ("--" `isPrefixOf` l)
    parseConstraint l = case simpleParse l of
                          Just c | depPkgName c == pn -> pure c
                                 | otherwise          -> failure ("constraint on unexpected package: " <> l)
                          Nothing                     -> failure ("cannot parse constraint: " <> l)
    failure msg = fail (unPackageName pn <> "/preferred-versions: " <> msg)

-- | Extract the @x-revision@ header added by Hackage.

packageRevision :: GenericPackageDescription -> Int
packageRevision = maybe 0 read . lookup "x-revision" . customFieldsPD . packageDescription

-- | Does the given Cabal build define a library component?

hasLibrary :: GenericPackageDescription -> Bool
hasLibrary gpd = isJust (library (packageDescription gpd)) || isJust (condLibrary gpd)

-- | Parse a Cabal file. The 'PackageIdentifier' argument is used only to
-- construct a more meaningful error message in case something goes wrong.

parseCabalFile :: MonadFail m => PackageIdentifier -> ByteString -> m GenericPackageDescription
parseCabalFile pid =
  maybe (fail (prettyShow pid <> ": cannot parse cabal file")) pure . parseGenericPackageDescriptionMaybe
