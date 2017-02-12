{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Oracle.PackageList where

import Orphans ()
import ParseUtils
import Types

import Data.Maybe
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Classes
import Distribution.Package
import Distribution.Text
import Distribution.Version

newtype GetPackageList = GetPackageList PackageSetId
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

-- TODO: Report "banned" backages that don't show up in the main package set.
-- TODO: Complain about "banned" backages that show up in the extra packages set.
-- TODO: Figure out a meaningful way how extra packages override default version choices.

getPackageList :: FilePath
               -> (Dependency -> Action Version)
               -> Rules (GetPackageList -> Action [PackageIdentifier])
getPackageList configDir resolveConstraint = addOracle $ \(GetPackageList (PackageSetId psid)) -> do
  pset <- parseCabalConfig <$> readFile' (configDir </> psid </> "stackage-packages.txt")
  banned <- readPackageNameList (configDir </> psid </> "banned-packages.txt")
  extra <- readConstraintList (configDir </> psid </> "extra-packages.txt")
  extra' <- forP extra $ \c@(Dependency pn _) -> PackageIdentifier pn <$> resolveConstraint c
  let pset'  = filter (\(PackageIdentifier pn _) -> pn `notElem` banned) pset
      pset'' = pset' ++ extra'
  return pset''

-- TODO: Recognize and consciously drop "foobar installed" lines; then
--       implement proper error handling, i.e. fail when something doesn't look
--       right.

parseCabalConfig :: String -> [PackageIdentifier]
parseCabalConfig buf = dependencyToId <$> catMaybes (parseCabalConfigLine <$> lines buf)
  where
    parseCabalConfigLine :: String -> Maybe Dependency
    parseCabalConfigLine ('-':'-':_) = Nothing
    parseCabalConfigLine ('c':'o':'n':'s':'t':'r':'a':'i':'n':'t':'s':':':l) = parseCabalConfigLine l
    parseCabalConfigLine (' ':l) = parseCabalConfigLine l
    parseCabalConfigLine l = simpleParse (if last l == ',' then init l else l)

    dependencyToId :: Dependency -> PackageIdentifier
    dependencyToId d@(Dependency n vr) = PackageIdentifier n v
      where v   = fromMaybe err (isSpecificVersion vr)
            err = error ("dependencyToId: unexpected argument " ++ show d)
