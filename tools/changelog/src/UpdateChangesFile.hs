{-# LANGUAGE OverloadedStrings #-}

module UpdateChangesFile
  ( updateChangesFile, TimeStamp, EMail
  )
  where

import ExtractVersionUpdates
import GuessChangelog

import Control.Monad.Extra
import Data.Maybe
import qualified Data.Text as Text
import Text.PrettyPrint.HughesPJ as Pretty hiding ( (<>) )
import Data.Time.Format
import Distribution.Package
import Distribution.Pretty
import Distribution.Version
import Prelude hiding ( FilePath )
import System.Directory
import Turtle hiding ( x, l, text )

type TimeStamp = Text
type EMail = Text

updateChangesFile :: Maybe TimeStamp -> FilePath -> PackageName -> Version -> EMail -> IO ()
updateChangesFile now' changesFile pkg newv email =
  ifM (notM (testfile changesFile)) (commit mempty) $ do
    oldVs <- extractVersionUpdates (Text.unpack (format fp changesFile))
    -- debug (fp%" mentions these version updates: "%wpl%"\n") changesFile oldVs
    when (null oldVs) (die (format ("cannot determine previous version number "%fp%" from") changesFile))
    let oldv = head oldVs
    when (oldv > newv) (die (format (fp%": unsupprted downgrade from version "%wp%" to "%wp) changesFile oldv newv))
    sh $ unless (oldv == newv) $ do
      let oldpkg = format (wp%"-"%wp) pkg oldv
          newpkg = format (wp%"-"%wp) pkg newv
      systemTempDir <- fromString <$> liftIO getTemporaryDirectory
      tmpDir <- mktempdir systemTempDir "update-changes-file-XXXXXX"
      procs "cabal" ["unpack", "-v0", format ("--destdir="%fp) tmpDir, "--", oldpkg] mempty
      procs "cabal" ["unpack", "-v0", format ("--destdir="%fp) tmpDir, "--", newpkg] mempty
      gcl <- liftIO (guessChangelog (tmpDir </> fromText oldpkg) (tmpDir </> fromText newpkg))
      case gcl of
        Right txt -> liftIO (commit txt)
        Left desc -> liftIO (commit (Text.pack (renderChangeLog (prettyGuessedChangeLog (pkg,oldv,newv) desc))))

  where
    commit :: Text -> IO ()
    commit  cl' = do
      now <- maybe (Text.pack . formatTime defaultTimeLocale changeLogDateFormat <$> date) return now'
      txt <- readTextFile changesFile <|> pure ""
      let cl = Text.unlines (map (\l -> if Text.null l then l else "  " <> l) (Text.lines cl'))
      writeTextFile changesFile $ format
        ("-------------------------------------------------------------------\n\
         \"%s%" - "%s%"\n\
         \\n\
         \- "%s%" "%wp%" "%s%" version "%wp%".\n"%s%"\n"%s)
        now email
        (if Text.null cl' then "Add" else "Update")
        pkg
        (if Text.null cl' then "at" else "to")
        newv cl txt

wp :: Pretty a => Format r (a -> r)
wp = makeFormat (Text.pack . prettyShow)

-- | Appropriate format parameter for 'formatTime' and 'parseTimeM'.
changeLogDateFormat :: String
changeLogDateFormat = "%a %b %e %H:%M:%S %Z %Y"

prettyGuessedChangeLog :: (PackageName, Version, Version) -> GuessedChangelog -> Doc

prettyGuessedChangeLog _ (UndocumentedUpdate p) = para $
        "Upstream has not updated the file " ++ show (Text.unpack (format fp p)) ++ " since the last release."

prettyGuessedChangeLog _ NoChangelogFiles = para "Upstream does not provide a change log file."

prettyGuessedChangeLog _ (NoCommonChangelogFiles old new)
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

prettyGuessedChangeLog _ (MoreThanOneChangelogFile _) = error "more than one"

prettyGuessedChangeLog ctx (UnmodifiedTopIsTooLarge p _) = para $
        "Upstream's change log file format is strange (too much unmodified text at\n\
        \at the top). The automatic updater cannot extract the relevant additions.\n\
        \You can find the file at: " ++ hackageCLUrl ctx p

prettyGuessedChangeLog ctx (NotJustTopAdditions p) = para $
        "Upstream has edited the change log file since the last release in a non-trivial\n\
        \way, i.e. they did more than just add a new entry at the top. You can review the\n\
        \file at: " ++ hackageCLUrl ctx p

hackageCLUrl :: (PackageName,Version,Version) -> FilePath -> String
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
