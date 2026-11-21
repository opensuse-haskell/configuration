{-# LANGUAGE OverloadedStrings #-}

-- | The specification of a package set exactly as it is written in a YAML
-- file. 'PackageSetSpecRaw' keeps every entry of the file in its original
-- order, including duplicates; nothing is checked for consistency here. That
-- happens when the raw specification is turned into a proper
-- 'Config.PackageSetSpec.PackageSetSpec'.

module Config.PackageSetSpecRaw ( PackageSetSpecRaw(..), readPackageSetSpecRaw ) where

import MyCabal
import Types

import Control.Monad ( unless )
import Data.Aeson.Key ( Key )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types ( formatPath )
import qualified Data.List as List
import Data.Yaml
import Data.Yaml.Internal ( Warning(..) )

-- | A package set specification as read from a file. Unlike
-- 'Config.PackageSetSpec.PackageSetSpec', it uses plain lists, so it doesn't lose entries
-- that are listed more than once.

data PackageSetSpecRaw = PackageSetSpecRaw
  { rawCompiler              :: CompilerId                       -- ^ @compiler@
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
fieldNames = [ "compiler", "core-packages", "target-packages", "package-set"
             , "allow-newer", "flags", "forced-executables", "pkg-config-dependencies" ]

-- | All fields are required, and unknown fields are an error, because their
-- contents would be ignored otherwise.

instance FromJSON PackageSetSpecRaw where
  parseJSON = withObject "package set specification" $ \o -> do
    let unknown = filter (`notElem` fieldNames) (KeyMap.keys o)
    unless (null unknown) $
      fail ("unknown fields: " <> unwords (Key.toString <$> unknown))
    PackageSetSpecRaw
      <$> (o .: "compiler" >>= parseText "CompilerId")
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

readPackageSetSpecRaw :: FilePath -> IO PackageSetSpecRaw
readPackageSetSpecRaw path = do
  result <- decodeFileWithWarnings path
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
