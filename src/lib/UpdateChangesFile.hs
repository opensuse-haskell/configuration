{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module UpdateChangesFile
  ( updateChangesFile, TimeStamp, parseTimeStamp, EMail, Revision
    -- * Internals exposed for testing
  , latestTimeStamp, nextTimeStamp, indentEntryText
  )
  where

import ExtractVersionUpdates
import MyCabal

import Control.Monad.Extra
import Data.Maybe
import Data.Time.Clock ( addUTCTime )
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Format
import OpenSuse.GuessChangeLog
import OpenSuse.Prelude
import qualified Prelude
import System.Directory
import Text.PrettyPrint.HughesPJ as Pretty hiding ( (<>) )
import Turtle hiding ( x, l, text, stdout, stderr )

type TimeStamp = UTCTime
type EMail = Text

updateChangesFile :: Maybe TimeStamp -> Prelude.FilePath -> PackageName -> (Version, Revision) -> EMail -> IO ()
updateChangesFile now' changesFile' pkg (newv,newrv) email =
  ifM (notM (testfile changesFile)) (commit mempty) $ do
    oldVs <- extractVersionUpdates (Text.unpack (format fp changesFile))
    (oldv, oldrv) <- case oldVs of
                       []  -> die (format ("cannot determine previous version number "%fp%" from") changesFile)
                       x:_ -> pure x
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
        -- TODO: hard-coding a 10 line maximum header length here is probably a bad idea
        gcl <- liftIO (guessChangeLog 10 (tmpDir </> Text.unpack oldpkg) (tmpDir </> Text.unpack newpkg))
        case gcl of
          GuessedChangeLog _ txt -> liftIO (commit txt)
          _                      -> liftIO (commit (Text.pack (renderChangeLog (prettyGuessedChangeLog (pkg,oldv,newv) gcl))))

  where
    changesFile = fromString changesFile'

    commit :: Text -> IO ()
    commit  cl' = do
      txt <- Text.readFile changesFile <|> pure ""
      now <- maybe date pure now'
      -- Entries must be in chronological order, but the time stamp we're
      -- given may be older than the topmost entry, e.g. because that entry was
      -- added manually. In that case, we date the new entry one second after
      -- the topmost one.
      latest <- either (die . format (fp%": "%s) changesFile) pure (latestTimeStamp txt)
      let ts = nextTimeStamp now latest
          cl = indentEntryText cl'
      Text.writeFile changesFile $ format
        ("-------------------------------------------------------------------\n\
         \"%s%" - "%s%"\n\
         \\n\
         \- "%s%" "%wp%" "%s%" version "%wp%s%".\n"%s%"\n"%s)
        (Text.pack (formatTime defaultTimeLocale changeLogDateFormat ts)) email
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

-- | Given the desired time stamp of a new entry and the time stamp of the
-- topmost existing entry, if any, determine the time stamp the new entry
-- should get. To keep the change log in chronological order, the new entry is
-- dated at least one second after the topmost one.
nextTimeStamp :: TimeStamp -> Maybe TimeStamp -> TimeStamp
nextTimeStamp now = maybe now (max now . addUTCTime 1)

-- | Indent the body text of a change log entry, stripping trailing whitespace
-- from every line. Empty lines stay empty.
indentEntryText :: Text -> Text
indentEntryText = Text.unlines . map (indent . Text.stripEnd) . Text.lines
  where
    indent l = if Text.null l then l else "  " <> l

-- | Parse a time stamp in the format we use in change log entries, e.g.
-- @Mon Mar  9 11:47:45 UTC 2026@.
parseTimeStamp :: Text -> Maybe TimeStamp
parseTimeStamp = parseTimeM False defaultTimeLocale changeLogDateFormat . Text.unpack . Text.strip

-- | Determine the time stamp of the topmost entry in the given change log.
-- Gives 'Nothing' if the log has no entries and fails if the topmost entry's
-- time stamp cannot be parsed.
latestTimeStamp :: Text -> Either Text (Maybe TimeStamp)
latestTimeStamp txt =
  case dropWhile (not . isSeparator) (Text.lines txt) of
    _ : hdr : _ -> let (ts, _) = Text.breakOn " - " hdr
                      in maybe (Left ("cannot parse time stamp of the latest entry: " <> Text.strip hdr)) (Right . Just) (parseTimeStamp ts)
    _ -> Right Nothing
  where
    isSeparator l = "-----" `Text.isPrefixOf` l

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
