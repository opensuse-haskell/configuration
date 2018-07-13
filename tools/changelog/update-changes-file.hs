{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import ExtractVersionUpdates
import GuessChangelog

import Control.Monad.Extra
import Data.Maybe
import Data.Optional ( Optional )
-- import qualified Data.Set as Set
import qualified Data.Text as Text
import Text.PrettyPrint.HughesPJ as Pretty hiding ( (<>) )
import Data.Time.Format
import Distribution.Package
import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Version
import Prelude hiding ( FilePath )
import System.Directory
-- import System.Environment
import Turtle hiding ( x, l, text )

type TimeStamp = Text
type EMail = Text
type Options = (Maybe TimeStamp, FilePath, PackageName, Version, EMail)

parser :: Parser Options
parser = do
  now <- optional (optText "timestamp" 't' "timestamp for the generated change log entry")
  changesFile <- argPath "changes-file" "package change log file to update"
  pkg <- argParsec "package-name" "name of the Hackage package"
  vers <- argParsec "new-version" "version string of the updated package"
  email <- argText "e-mail" "e-mail address of the revision author"
  pure (now, changesFile, pkg, vers, email)

main :: IO ()
main = do
  opts@(_,changesFile,pkg,newv,_) <- options "Guess the change log entry between two version of a package." parser
  ifM (notM (testfile changesFile)) (updateChangesFile opts mempty) $ do
    oldVs <- extractVersionUpdates (Text.unpack (format fp changesFile))
    -- debug (fp%" mentions these version updates: "%wpl%"\n") changesFile oldVs
    if null oldVs then updateChangesFile opts mempty else sh $ do
      let oldv = head oldVs
      unless (oldv == newv) $ do
        let oldpkg = format (wp%"-"%wp) pkg oldv
            newpkg = format (wp%"-"%wp) pkg newv
        systemTempDir <- fromString <$> liftIO getTemporaryDirectory
        tmpDir <- mktempdir systemTempDir "update-changes-file-XXXXXX"
        procs "cabal" ["unpack", "-v0", format ("--destdir="%fp) tmpDir, "--", oldpkg] mempty
        procs "cabal" ["unpack", "-v0", format ("--destdir="%fp) tmpDir, "--", newpkg] mempty
        gcl <- liftIO (guessChangelog (tmpDir </> fromText oldpkg) (tmpDir </> fromText newpkg))
        case gcl of
          Right txt -> liftIO (updateChangesFile opts txt)
          Left desc -> liftIO (updateChangesFile opts (Text.pack (renderChangeLog (prettyGuessedChangeLog (pkg,oldv,newv) desc))))

updateChangesFile :: Options -> Text -> IO ()
updateChangesFile (now',changesFile,pkg,vers,email) cl' = do
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
    vers cl txt

argParsec :: Parsec a => ArgName -> Optional HelpMessage -> Parser a
argParsec argname help = arg (simpleParsec . Text.unpack) argname help

wp :: Pretty a => Format r (a -> r)
wp = makeFormat (Text.pack . prettyShow)

-- wpl :: Pretty a => Format r ([a] -> r)
-- wpl = makeFormat (Text.intercalate ", " . map (Text.pack . prettyShow))

-- | Appropriate format parameter for 'formatTime' and 'parseTimeM'.
changeLogDateFormat :: String
changeLogDateFormat = "%a %b %e %H:%M:%S %Z %Y"

prettyGuessedChangeLog :: (PackageName, Version, Version) -> GuessedChangelog -> Doc

prettyGuessedChangeLog _ (UndocumentedUpdate p) = para $
        "Upstream has not updated the file " ++ show (Text.unpack (format fp p)) ++ " since the last release."

prettyGuessedChangeLog _ NoChangelogFiles = para "Upstream does not provide a change log file."

prettyGuessedChangeLog _ (NoCommonChangelogFiles old new)
  | not (null old) && null new = para
        "Upstream has removed the change log file they used to maintain before from\n\
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

-- pluralS :: Foldable t => t a -> Text
-- pluralS a = if length a > 1
--                then "s"
--                else ""

renderChangeLog :: Doc -> String
renderChangeLog = renderStyle changeLogStyle

changeLogStyle :: Style
changeLogStyle = style { lineLength = 65, ribbonsPerLine = 1 }
