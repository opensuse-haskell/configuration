module Config.Tools ( makePackageSet, ppPackageSet, ppConstraintSet, updateTargetPackages ) where

import Config.Compiler
import Types
import MyCabal

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.PrettyPrint hiding ( (<>) )


-- | Build the package set from the given list of entries and sanity-check it
-- before it is used to generate builds. Aborts with an 'error' if the set is
-- inconsistent. The checks detect:
--
--  * Packages that are listed more than once. Converting the list into a map
--    would silently keep only the last of those entries, so this check has to
--    see the list rather than the finished 'PackageSet'.
--
--  * Packages that appear both in the package set and in the list of core
--    packages, 'ghcCorePackages'. Core packages are bundled with GHC itself
--    and are provided by the @ghc@ build, so generating a separate
--    @ghc-<name>@ build for one of them would produce a second, conflicting
--    copy of that library.
--
-- The error messages name every offending package.

makePackageSet :: [(PackageName, Version)] -> PackageSet
makePackageSet entries
  | not (Map.null duplicates) = error ("duplicate entries in package set: " <> List.intercalate ", " [ unPackageName pn <> " (" <> List.intercalate ", " (prettyShow <$> vs) <> ")" | (pn, vs) <- Map.toList duplicates ])
  | not (Set.null corePackagesInPackageSet) = error ("core packages listed in package set: " <> List.intercalate ", " (unPackageName <$> Set.toList corePackagesInPackageSet))
  | otherwise = packageSet
  where
    packageSet = Map.fromList entries
    duplicates = Map.filter ((> 1) . length) (Map.fromListWith (flip (<>)) [ (pn, [v]) | (pn, v) <- entries ])
    corePackagesInPackageSet = Map.keysSet packageSet `Set.intersection` Map.keysSet ghcCorePackages

-- | Pretty-print a package set as a list of quoted package identifiers, one
-- per line, with leading commas:
--
-- > [ "DAV-1.3.4"
-- > , "Decimal-0.5.2"
-- > ]

ppPackageSet :: PackageSet -> Doc
ppPackageSet ps = ppList [ unPackageName pn <> "-" <> prettyShow v | (pn, v) <- Map.toList ps ]

-- | Pretty-print a constraint set in the same style as 'ppPackageSet':
--
-- > [ "alex >=3.5.4.2"
-- > , "happy >=2.2"
-- > ]

ppConstraintSet :: ConstraintSet -> Doc
ppConstraintSet cs = ppList [ display (PackageVersionConstraint pn vr) | (pn, vr) <- Map.toList cs ]

-- | Print strings as a list of quoted entries, one per line, with leading
-- commas.

ppList :: [String] -> Doc
ppList entries = case doubleQuotes . text <$> entries of
                   []     -> text "[]"
                   e : es -> vcat ((text "[ " <> e) : map (text ", " <>) es) $$ rbrack

-- | Replace the version range of every target package with a lower bound on
-- the version that the given package set contains, i.e. @>= x.y.z@. Aborts
-- with an 'error' that names all target packages missing from the package
-- set.

updateTargetPackages :: ConstraintSet -> PackageSet -> ConstraintSet
updateTargetPackages targets ps
  | not (Set.null missing) = error ("target packages missing from package set: " <> List.intercalate ", " (unPackageName <$> Set.toList missing))
  | otherwise = orLaterVersion <$> ps `Map.restrictKeys` Map.keysSet targets
  where
    missing = Map.keysSet targets `Set.difference` Map.keysSet ps
