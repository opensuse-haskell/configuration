{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module UpdateChangesFile
  ( updateChangesFile, TimeStamp, EMail, Revision
  )
  where

import ExtractVersionUpdates
import MyCabal

import Control.Monad.Extra
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Format
import OpenSuse.GuessChangeLog
import OpenSuse.Prelude
import qualified Prelude
import System.Directory
import Text.PrettyPrint.HughesPJ as Pretty hiding ( (<>) )
import Turtle hiding ( x, l, text, stdout, stderr )

type TimeStamp = Text
type EMail = Text

updateChangesFile :: Maybe TimeStamp -> Prelude.FilePath -> PackageName -> (Version, Revision) -> EMail -> IO ()
updateChangesFile now' changesFile' pkg (newv,newrv) email =
  ifM (notM (testfile changesFile)) (commit mempty) $ do
    oldVs <- extractVersionUpdates (Text.unpack (format fp changesFile))
    when (null oldVs) (die (format ("cannot determine previous version number "%fp%" from") changesFile))
    let (oldv, oldrv) = head oldVs
    when (oldv > newv) (die (format (fp%": unsupported downgrade from version "%wp%" to "%wp) changesFile oldv newv))
    if oldv == newv
      then if | oldrv > newrv -> die (format (fp%": unsupported downgrade from revision "%wp%" to "%wp) changesFile oldrv newrv)
              | oldrv < newrv -> liftIO (commit "Upstream has revised the Cabal build instructions on Hackage.")
              | otherwise     -> pure ()
      else sh $ unless (oldv == newv) $ do
        let oldpkg = format (wp%"-"%wp) pkg oldv
            newpkg = format (wp%"-"%wp) pkg newv
        systemTempDir <- fromString <$> liftIO getTemporaryDirectory
        tmpDir <- mktempdir systemTempDir "update-changes-file-XXXXXX"
        procs "cabal" ["unpack", "-v0", format ("--destdir="%fp) tmpDir, "--", oldpkg] mempty
        procs "cabal" ["unpack", "-v0", format ("--destdir="%fp) tmpDir, "--", newpkg] mempty
        gcl <- liftIO (guessChangeLog (tmpDir </> Text.unpack oldpkg) (tmpDir </> Text.unpack newpkg))
        case gcl of
          GuessedChangeLog _ txt -> liftIO (commit txt)
          _                      -> liftIO (commit (Text.pack (renderChangeLog (prettyGuessedChangeLog (pkg,oldv,newv) gcl))))

  where
    changesFile = fromString changesFile'

    commit :: Text -> IO ()
    commit  cl' = do
      now <- maybe (Text.pack . formatTime defaultTimeLocale changeLogDateFormat <$> date) pure now'
      txt <- Text.readFile changesFile <|> pure ""
      let cl = Text.unlines (map (\l -> if Text.null l then l else "  " <> l) (Text.lines cl'))
      Text.writeFile changesFile $ format
        ("-------------------------------------------------------------------\n\
         \"%s%" - "%s%"\n\
         \\n\
         \- "%s%" "%wp%" "%s%" version "%wp%s%".\n"%s%"\n"%s)
        now email
        (if Text.null cl' then "Add" else "Update")
        pkg
        (if Text.null cl' then "at" else "to")
        newv
        (if newrv == 0 then "" else format (" revision "%d) newrv)
        cl txt

wp :: Pretty a => Format r (a -> r)
wp = makeFormat (Text.pack . prettyShow)

-- | Appropriate format parameter for 'formatTime' and 'parseTimeM'.
changeLogDateFormat :: String
changeLogDateFormat = "%a %b %e %H:%M:%S %Z %Y"

prettyGuessedChangeLog :: (PackageName, Version, Version) -> GuessedChangeLog -> Doc

prettyGuessedChangeLog _ (GuessedChangeLog _ _) = error
        "prettyGuessedChangeLog is not supposed to be used to format guessed entries"

prettyGuessedChangeLog _ (UndocumentedUpdate p) = para $
        "Upstream has not updated the file " ++ show (Text.unpack (format fp p)) ++ " since the last release."

prettyGuessedChangeLog _ NoChangeLogFiles = para "Upstream does not provide a change log file."

prettyGuessedChangeLog _ (NoCommonChangeLogFiles old new)
  | not (null old) && null new = para
        "Upstream has removed the change log file they used to maintain from\n\
        \the distribution tarball."
  | null old && not (null new) = para
        "Upstream added a new change log file in this release. With no previous\n\
        \version to compare against, the automatic updater cannot reliable determine\n\
        \the relevante entries for this release."
  | otherwise = para
        "Upstream has renamed and modified the change log file(s) in this release.\n\
        \Unfortunately, the automatic updater cannot reliable determine relevant\n\
        \entries for this release."

prettyGuessedChangeLog _ (MoreThanOneChangeLogFile ps) = error ("more than one" ++ show ps)

prettyGuessedChangeLog ctx (UnmodifiedTopIsTooLarge p _) = para $
        "Upstream's change log file format is strange (too much unmodified text at\n\
        \at the top). The automatic updater cannot extract the relevant additions.\n\
        \You can find the file at: " ++ hackageCLUrl ctx p

prettyGuessedChangeLog ctx (NotJustTopAdditions p) = para $
        "Upstream has edited the change log file since the last release in a non-trivial\n\
        \way, i.e. they did more than just add a new entry at the top. You can review the\n\
        \file at: " ++ hackageCLUrl ctx p

hackageCLUrl :: (PackageName,Version,Version) -> Turtle.FilePath -> String
hackageCLUrl (pkg,_,newv) path = concat
  [ "http://hackage.haskell.org/package/"
  , prettyShow pkg, "-", prettyShow newv
  , "/src/", Text.unpack (format fp path)
  ]

para :: String -> Doc
para = fsep . map text . words

renderChangeLog :: Doc -> String
renderChangeLog = renderStyle changeLogStyle

changeLogStyle :: Style
changeLogStyle = style { lineLength = 65, ribbonsPerLine = 1 }
