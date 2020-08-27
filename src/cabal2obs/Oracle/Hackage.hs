{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Oracle.Hackage where

import OpenSuse.Prelude
import Orphans ()

import Codec.Archive.Tar.Index ( TarIndex, TarIndexEntry(..) )
import Codec.Archive.Tar.Entry as Tar ( entryContent, EntryContent(..) )
import qualified Codec.Archive.Tar.Index as Tar
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Types.PackageVersionConstraint
import Distribution.Version
import System.Directory
import System.IO

type Constraint = PackageVersionConstraint

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
            Just (idx,rest) | BSS.null rest -> return (Hackage idx)
                            | otherwise     -> fail (idxFile <> ": unexpected " <> show (BSS.length rest) <> " extra bytes at the end")

type instance RuleResult Constraint = Version

-- | Resolve a Hackage 'PackageVersionConstraint' into the latest 'Version'
-- that satisfies the constraint or report an error via 'fail'. This oracle
-- needs access to 'Hackage' through a registered 'addHackageCache' action.

addConstraintResolverOracle :: Action Hackage -> Rules (Constraint -> Action Version)
addConstraintResolverOracle hackage = addOracle $ \(PackageVersionConstraint name vrange) -> do
  Hackage idx <- hackage
  let n = unPackageName name
  es <- case Tar.lookup idx n of
          Nothing               -> fail (n <> " is not a valid Hackage package")
          Just (TarFileEntry _) -> fail ("looking up " <> show n <> " in Hackage gives unexpected file(!) result")
          Just (TarDir es)      -> return es
  return $ maximum [ v | (p, TarDir _) <- es, v <- [fromString p], v `withinRange` vrange ]

-- | Helper function that maps a given 'PackageIdentifier' into the 'FilePath'
-- of the corresponding Cabal file. The first function argument gives the
-- location of a checked out copy of the
-- <https://github.com/commercialhaskell/all-cabal-files> repository.

cabalFilePath :: PackageIdentifier -> FilePath
cabalFilePath (PackageIdentifier n v) =
  unPackageName n </> prettyShow v </> unPackageName n <.> "cabal"

-- | Cached access to parsed ADT that represent the Cabal files registered in
-- Hackage.

addCabalFileCache :: Action Hackage -> Rules (PackageIdentifier -> Action ByteString)
addCabalFileCache hackage = newCache $ \pid -> do
  Hackage idx <- hackage
  off <- case Tar.lookup idx (cabalFilePath pid) of
    Nothing                 -> fail (prettyShow pid <> " is not a valid Hackage package")
    Just (TarFileEntry off) -> return off
    Just (TarDir _)         -> fail ("looking up " <> prettyShow pid <> " in Hackage gives unexpected directory(!) result")
  home <- liftIO (getAppUserDataDirectory "cabal")
  let tarball = home </> "packages/hackage.haskell.org/01-index.tar"
  need [tarball]
  e <- liftIO $ withFile tarball ReadMode (`Tar.hReadEntry` off)
  case Tar.entryContent e of
    NormalFile buf _ -> return (BSL.toStrict buf)
    x                -> fail (prettyShow pid <> ": unexpected entry type in tarball: " <> show x)

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
  maybe (fail (prettyShow pid <> ": cannot parse cabal file")) return . parseGenericPackageDescriptionMaybe
