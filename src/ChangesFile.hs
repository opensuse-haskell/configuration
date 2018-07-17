module ChangesFile where

import Data.Time
import Distribution.Package
import Distribution.Pretty

type EMailAddress = String
type Revision = Int

-- | Create a change log entry for a Haskell package update.

mkChangeEntry :: PackageIdentifier -> Revision -> EMailAddress -> IO String
mkChangeEntry (PackageIdentifier pn v) rev email = do
  ts <- formatTime defaultTimeLocale "%a %b %_d %H:%M:%S %Z %Y" <$> getCurrentTime
  let revisionString = if rev > 0 then "revision " ++ show rev else ""
      blank          = if null revisionString then "" else " "
      versionString  = prettyShow v ++ blank ++ revisionString
  return $ unlines
    [ "-------------------------------------------------------------------"
    , unwords [ ts, "-", email ]
    , ""
    , "- Update " ++ unPackageName pn ++ " to version " ++ versionString ++ "."
    , ""
    ]
