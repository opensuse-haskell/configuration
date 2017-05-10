{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Oracle.FlagAssignment where

import Orphans ()
import ParseUtils
import Types

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Classes
import Distribution.Simple.Utils ( lowercase )
import Distribution.PackageDescription

newtype GetFlagAssignments = GetFlagAssignments PackageSetId
  deriving (Show, Eq, Ord, Hashable, NFData, Binary)

getFlagAssignments :: FilePath -> Rules (GetFlagAssignments -> Action [(String,FlagAssignment)])
getFlagAssignments configDir = addOracle $ \(GetFlagAssignments (PackageSetId psid)) ->
  readFlagAssignents (configDir </> psid </> "flag-assignment.txt")

readFlagAssignents :: FilePath -> Action [(String,FlagAssignment)]
readFlagAssignents p = fmap (fmap (readFlagList . words . tail) . break (==':')) <$> readConfigFile p

readFlagList :: [String] -> FlagAssignment
readFlagList = map (tagWithValue . noMinusF)
  where
    tagWithValue ('-':fname) = (FlagName (lowercase fname), False)
    tagWithValue fname       = (FlagName (lowercase fname), True)

    noMinusF :: String -> String
    noMinusF ('-':'f':_) = error "don't use '-f' in flag assignments; just use the flag's name"
    noMinusF x           = x
