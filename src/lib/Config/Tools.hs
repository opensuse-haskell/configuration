module Config.Tools ( makeMap, makeSet, getPackageSetName, ppPackageSet, ppConstraintSet, updateTargetPackages ) where

import Types
import MyCabal

import Control.Monad ( unless )
import qualified Data.List as List
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import Text.PrettyPrint hiding ( (<>) )
import Text.PrettyPrint.HughesPJClass ( Pretty )


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

-- | Pretty-print a package set as a list of quoted package identifiers, one
-- per line, with leading commas, sorted by name regardless of case:
--
-- > [ "DAV-1.3.4"
-- > , "Decimal-0.5.2"
-- > ]

ppPackageSet :: PackageSet -> Doc
ppPackageSet ps = ppList [ unPackageName pn <> "-" <> prettyShow v | (pn, v) <- List.sortOn (lowercase . unPackageName . fst) (Map.toList ps) ]

-- | Pretty-print a constraint set in the same style and order as
-- 'ppPackageSet':
--
-- > [ "alex >=3.5.4.2"
-- > , "happy >=2.2"
-- > ]

ppConstraintSet :: ConstraintSet -> Doc
ppConstraintSet cs = ppList [ display (PackageVersionConstraint pn vr) | (pn, vr) <- List.sortOn (lowercase . unPackageName . fst) (Map.toList cs) ]

-- | Print strings as a list of quoted entries, one per line, with leading
-- commas.

ppList :: [String] -> Doc
ppList entries = case doubleQuotes . text <$> entries of
                   []     -> text "[]"
                   e : es -> vcat ((text "[ " <> e) : map (text ", " <>) es) $$ rbrack

-- | Replace the version range of every target package with a lower bound on
-- the version that the given package set contains, i.e. @>= x.y.z@. Fails
-- with a message that names all target packages missing from the package set.

updateTargetPackages :: MonadFail m => ConstraintSet -> PackageSet -> m ConstraintSet
updateTargetPackages targets ps
  | not (Set.null missing) = fail ("target packages missing from package set: " <> List.intercalate ", " (unPackageName <$> Set.toList missing))
  | otherwise = pure (orLaterVersion <$> ps `Map.restrictKeys` Map.keysSet targets)
  where
    missing = Map.keysSet targets `Set.difference` Map.keysSet ps
