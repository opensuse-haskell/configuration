{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PackageSetSpec ( PackageSetSpec(..), readPackageSetSpec ) where

import MyCabal
import Types

import Control.Monad ( unless )
import Control.Monad.IO.Class
import Data.Aeson.Key ( Key )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types ( formatPath )
import qualified Data.List as List
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Yaml
import Data.Yaml.Internal ( Warning(..) )
import Development.Shake.Classes
import GHC.Generics ( Generic )
import System.Environment ( lookupEnv )
import Text.PrettyPrint.HughesPJClass ( Pretty )

-- | Everything that defines a package set.

data PackageSetSpec = PackageSetSpec
  { packageSetName        :: FilePath                       -- ^ The name of the package set, e.g. @ghc-9.14.x@, derived from the compiler.
  , maintainer            :: Text                           -- ^ Name and e-mail address of the maintainer, e.g. for new .changes entries.
  , compiler              :: CompilerId                     -- ^ The compiler we build with.
  , corePackages          :: PackageSet                     -- ^ The packages bundled with the compiler.
  , packageSet            :: PackageSet                     -- ^ The packages we generate builds for.
  , targetPackages        :: ConstraintSet                  -- ^ The packages the package set is resolved for.
  , allowNewer            :: Map PackageName PackageNameSet -- ^ Version bounds we ignore when resolving the package set.
  , flags                 :: Map PackageName FlagAssignment -- ^ The flags we configure for packages.
  , forcedExecutables     :: PackageNameSet                 -- ^ Packages we build as executables even though they have a library.
  , pkgConfigDependencies :: Set NixpkgsAttribute           -- ^ System libraries that packages look up via pkg-config.
  }
  deriving (Show, Eq, Generic, Hashable, Binary, NFData)

-- | Read the specification of a package set from the given YAML file. Fails
-- if the file can't be read or if its contents are inconsistent, e.g. if a list contains duplicates, a package
-- identifier has no version, a flag is assigned more than once for the same
-- package, or a core package is listed in the package set.
--
-- If the environment variable @CABAL2OBS_MAINTAINER@ is set, its value
-- overrides the maintainer given in the file.

readPackageSetSpec :: (MonadIO m, MonadFail m) => FilePath -> m PackageSetSpec
readPackageSetSpec path = do
  let context what = what <> " in " <> path
  PackageSetSpecRaw{..} <- readPackageSetSpecRaw path
  maintainer <- maybe rawMaintainer Text.pack <$> liftIO (lookupEnv "CABAL2OBS_MAINTAINER")
  let compiler = rawCompiler
  packageSetName        <- getPackageSetName compiler
  corePackages          <- makeMap (context "core-packages") [ (pkgName p, pkgVersion p) | p <- rawCorePackages ]
  packageSet            <- makeMap (context "package-set") [ (pkgName p, pkgVersion p) | p <- rawPackageSet ]
  targetPackages        <- makeMap (context "target-packages") [ (pn, vr) | Dependency pn vr _ <- rawTargetPackages ]
  allowNewer            <- makeMap (context "allow-newer") =<<
                             traverse (\(pn, deps) -> (,) pn <$> makeSet (context ("allow-newer of " <> unPackageName pn)) deps) rawAllowNewer
  flags                 <- makeMap (context "flags") rawFlags
  forcedExecutables     <- makeSet (context "forced-executables") rawForcedExecutables
  pkgConfigDependencies <- makeSet (context "pkg-config-dependencies") rawPkgConfigDependencies
  let unversioned = [ "package identifiers without a version in " <> context section <> ": " <> List.intercalate ", " names
                    | (section, pids) <- [("core-packages", rawCorePackages), ("package-set", rawPackageSet)]
                    , let names = [ unPackageName (pkgName p) | p <- pids, null (versionNumbers (pkgVersion p)) ]
                    , not (null names)
                    ]
  unless (null unversioned) $
    fail (List.intercalate "; " unversioned)
  let duplicateFlags = [ unPackageName pn <> " (" <> List.intercalate ", " (unFlagName <$> fs) <> ")"
                       | (pn, fa) <- rawFlags, let fs = findDuplicateFlagAssignments fa, not (null fs) ]
  unless (null duplicateFlags) $
    fail ("flags assigned more than once in " <> context "flags" <> ": " <> List.intercalate ", " duplicateFlags)
  let corePackagesInPackageSet = Map.keysSet packageSet `Set.intersection` Map.keysSet corePackages
  unless (Set.null corePackagesInPackageSet) $
    fail ("core packages listed in " <> context "package-set" <> ": " <> List.intercalate ", " (unPackageName <$> Set.toList corePackagesInPackageSet))
  pure PackageSetSpec{..}

----- YAML interface ----------------------------------------------------------

-----------------------------------------------------------------------------
-- Helpers for turning the raw specification into a proper one
-----------------------------------------------------------------------------

-- | Turn a list of key-value pairs into a map. Fails if a key occurs more than
-- once, because the map would silently keep only the last of those entries.
-- The first argument names the list in the error message, which lists all
-- duplicate keys.

makeMap :: (Ord a, Pretty a, MonadFail m) => String -> [(a, b)] -> m (Map a b)
makeMap what xs = do
  let counts = Map.fromListWith (+) [ (k, 1 :: Int) | (k, _) <- xs ]
      duplicates = Map.keys (Map.filter (> 1) counts)
  unless (null duplicates) $
    fail ("duplicate entries in " <> what <> ": " <> List.intercalate ", " (prettyShow <$> duplicates))
  pure (Map.fromList xs)

-- | Turn a list into a set. Fails if an element occurs more than once, like
-- 'makeMap'.

makeSet :: (Ord a, Pretty a, MonadFail m) => String -> [a] -> m (Set a)
makeSet what xs = Map.keysSet <$> makeMap what [ (x, ()) | x <- xs ]

-- | The name of the package set we build with the given compiler. It
-- consists of the compiler flavor and the major version, which for GHC are
-- the first two components of the version number:
--
-- >>> :set -XOverloadedStrings
-- >>> getPackageSetName "ghc-9.14.1.2.3.4"
-- "ghc-9.14.x"
--
-- >>> getPackageSetName "ghc-9.14.1"
-- "ghc-9.14.x"
--
-- >>> getPackageSetName "ghc-10.2"
-- "ghc-10.2.x"
--
-- Fails if the version number has fewer than two components:
--
-- >>> getPackageSetName "ghc-9"
-- *** Exception: user error (compiler ghc-9 has no major version number with two components)
-- ...

getPackageSetName :: MonadFail m => CompilerId -> m FilePath
getPackageSetName cid@(CompilerId flavor v) =
  case versionNumbers v of
    x : y : _ -> pure (display flavor <> "-" <> show x <> "." <> show y <> ".x")
    _         -> fail ("compiler " <> display cid <> " has no major version number with two components")

-----------------------------------------------------------------------------
-- The specification exactly as it is written in a YAML file
--
-- 'PackageSetSpecRaw' keeps every entry of the file in its original order,
-- including duplicates; nothing is checked for consistency here. That happens
-- when 'readPackageSetSpec' turns the raw specification into a proper
-- 'PackageSetSpec'.
-----------------------------------------------------------------------------

-- | A package set specification as read from a file. Unlike
-- 'PackageSetSpec', it uses plain lists, so it doesn't lose entries
-- that are listed more than once.

data PackageSetSpecRaw = PackageSetSpecRaw
  { rawMaintainer            :: Text                             -- ^ @maintainer@
  , rawCompiler              :: CompilerId                       -- ^ @compiler@
  , rawCorePackages          :: [PackageIdentifier]              -- ^ @core-packages@
  , rawTargetPackages        :: [Constraint]                     -- ^ @target-packages@
  , rawPackageSet            :: [PackageIdentifier]              -- ^ @package-set@
  , rawAllowNewer            :: [(PackageName, [PackageName])]   -- ^ @allow-newer@
  , rawFlags                 :: [(PackageName, FlagAssignment)]  -- ^ @flags@
  , rawForcedExecutables     :: [PackageName]                    -- ^ @forced-executables@
  , rawPkgConfigDependencies :: [NixpkgsAttribute]               -- ^ @pkg-config-dependencies@
  }
  deriving (Show, Eq)

-- | The fields of a package set specification file, in the order in which
-- they are written.

fieldNames :: [Key]
fieldNames = [ "maintainer", "compiler", "core-packages", "target-packages", "package-set"
             , "allow-newer", "flags", "forced-executables", "pkg-config-dependencies" ]

-- | All fields are required, and unknown fields are an error, because their
-- contents would be ignored otherwise.

instance FromJSON PackageSetSpecRaw where
  parseJSON = withObject "package set specification" $ \o -> do
    let unknown = filter (`notElem` fieldNames) (KeyMap.keys o)
    unless (null unknown) $
      fail ("unknown fields: " <> unwords (Key.toString <$> unknown))
    PackageSetSpecRaw
      <$> o .: "maintainer"
      <*> (o .: "compiler" >>= parseText "CompilerId")
      <*> (o .: "core-packages" >>= traverse (parseText "PackageIdentifier"))
      <*> (o .: "target-packages" >>= traverse (parseText "Dependency"))
      <*> (o .: "package-set" >>= traverse (parseText "PackageIdentifier"))
      <*> (o .: "allow-newer" >>= traverse (singleEntry (traverse (parseText "PackageName"))))
      <*> (o .: "flags" >>= traverse (singleEntry (parseText "FlagAssignment")))
      <*> (o .: "forced-executables" >>= traverse (parseText "PackageName"))
      <*> o .: "pkg-config-dependencies"

-- | Parse a mapping with exactly one entry, e.g. @botan-low: [base]@, whose
-- key is a package name. A sequence of such mappings keeps the order of its
-- entries and any duplicates, which a single mapping would not.

singleEntry :: FromJSON a => (a -> Parser b) -> Value -> Parser (PackageName, b)
singleEntry parseValue = withObject "mapping with one entry" $ \o ->
  case KeyMap.toList o of
    [(k, v)] -> (,) <$> parseText "PackageName" (Key.toString k) <*> (parseJSON v >>= parseValue)
    entries  -> fail ("expected a mapping with exactly one entry, but got " <> show (length entries))

-- | Read a package set specification from the given YAML file. Fails if the
-- file can't be parsed or if the parser reports any warnings, e.g. about
-- duplicate keys; the error message lists all of them.

readPackageSetSpecRaw :: (MonadIO m, MonadFail m) => FilePath -> m PackageSetSpecRaw
readPackageSetSpecRaw path = do
  result <- liftIO (decodeFileWithWarnings path)
  case result of
    Left err -> fail (path <> ": " <> prettyPrintParseException err)
    Right (warnings, raw) -> do
      unless (null warnings) $
        fail (path <> ": " <> List.intercalate ", " (describeWarning <$> warnings))
      pure raw

-- | Describe a warning of the YAML parser. Matching on every constructor makes
-- GHC point out warnings that newer versions of the yaml library add, so
-- that they can't be ignored by accident.

describeWarning :: Warning -> String
describeWarning (DuplicateKey jp) = "duplicate key " <> formatPath jp
