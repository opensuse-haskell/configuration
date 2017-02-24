{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Oracle.PackageList where

import Orphans ()
import ParseUtils
import Types

import Control.Monad
import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Distribution.Package
import Distribution.Text
import Distribution.Version

newtype GetPackageList = GetPackageList PackageSetId
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

-- TODO: Figure out a meaningful way how extra packages override default version choices.

getPackageList :: FilePath
               -> (Dependency -> Action Version)
               -> Rules (GetPackageList -> Action [PackageIdentifier])
getPackageList configDir resolveConstraint = addOracle $ \(GetPackageList (PackageSetId psid)) -> do
  pset <- parseCabalConfig <$> readFile' (configDir </> psid </> "stackage-packages.txt")
  banned <- readPackageNameList (configDir </> psid </> "banned-packages.txt")
  extra <- readConstraintList (configDir </> psid </> "extra-packages.txt")
  extra' <- forP extra $ \c@(Dependency pn _) -> PackageIdentifier pn <$> resolveConstraint c
  checkConsistency psid (packageName <$> pset) (packageName <$> extra') banned
  let pset'  = filter (\(PackageIdentifier pn _) -> pn `notElem` banned) pset
      pset'' = pset' ++ extra'
  return pset''

checkConsistency :: String -> [PackageName] -> [PackageName] -> [PackageName] -> Action ()
checkConsistency psid _ {-stackage-} extra banned = do
  let bogusBans  = extra `intersect` banned
  unless (null bogusBans) $ fail (psid ++ " bans extra packages: " ++ intercalate " " (display <$> bogusBans))

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
