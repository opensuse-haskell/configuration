{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Oracle.PackageList where

import Orphans ()
import Types
import Config

import Control.Monad
import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.Classes
import Distribution.Package
import Distribution.Text
import Distribution.Version

newtype GetPackageList = GetPackageList PackageSetId
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

type instance RuleResult GetPackageList = [PackageIdentifier]

-- TODO: Figure out a meaningful way how extra packages override default version choices.

getPackageList :: (Dependency -> Action Version)
               -> Rules (GetPackageList -> Action [PackageIdentifier])
getPackageList resolveConstraint = addOracle $ \(GetPackageList psid) -> do
  let pset = map dependencyToId (stackagePackages (getConfig psid))
      banned = bannedPackages (getConfig psid)
      extra = extraPackages (getConfig psid)
  extra' <- forP extra $ \c@(Dependency pn _) -> PackageIdentifier pn <$> resolveConstraint c
  checkConsistency psid (packageName <$> pset) (packageName <$> extra') banned
  let pset'  = filter (\(PackageIdentifier pn _) -> pn `notElem` banned) pset
      pset'' = pset' ++ extra'
  return pset''

checkConsistency :: PackageSetId -> [PackageName] -> [PackageName] -> [PackageName] -> Action ()
checkConsistency psid stackage extra banned = do
  let bogusBans  = extra `intersect` banned
  unless (null bogusBans) $ fail (show psid ++ " bans extra packages: " ++ unwords (display <$> bogusBans))
  let duplicate  = extra `intersect` stackage
  unless (null duplicate) $ fail (show psid ++ " lists stackage packages in extra: " ++ unwords (display <$> duplicate))
  -- TODO: This fails because of core packages etc.
  -- let unknownBanned = banned \\ (stackage `union` extra)
  -- unless (null unknownBanned) $ fail (show psid ++ " bans unknown packages: " ++ unwords (display <$> unknownBanned))


dependencyToId :: Dependency -> PackageIdentifier
dependencyToId d@(Dependency n vr) = PackageIdentifier n v
  where v   = fromMaybe err (isSpecificVersion vr)
        err = error ("dependencyToId: unexpected argument " ++ show d)
