module Config.Tools ( makePackageSet, ppPackageSet ) where

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
ppPackageSet ps = case entries of
                    []     -> text "[]"
                    e : es -> vcat ((text "[ " <> e) : map (text ", " <>) es) $$ rbrack
  where
    entries = [ doubleQuotes (text (unPackageName pn <> "-" <> prettyShow v)) | (pn, v) <- Map.toList ps ]
