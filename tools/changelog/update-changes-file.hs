{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import ExtractVersionUpdates
import GuessChangelog

import qualified Control.Foldl as Fold
import Control.Monad.Extra
import Data.Maybe
import Data.Optional ( Optional )
import Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Text as Text
-- import qualified Data.Text.IO as Text
-- import Data.Time.Clock
import Data.Time.Format
import Distribution.Package
import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Version
import Prelude hiding ( FilePath )
import System.Directory
-- import System.Environment
import Turtle hiding ( x )

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
    debug (fp%" mentions these version updates: "%wpl%"\n") changesFile oldVs
    if null oldVs then updateChangesFile opts mempty else sh $ do
      let oldv = head oldVs
      if oldv == newv then debug "no update; nothing to do\n" else do
        let oldpkg = format (wp%"-"%wp) pkg oldv
            newpkg = format (wp%"-"%wp) pkg newv
        systemTempDir <- fromString <$> liftIO getTemporaryDirectory
        tmpDir <- mktempdir systemTempDir "update-changes-file-XXXXXX"
        procs "cabal" ["unpack", "-v0", format ("--destdir="%fp) tmpDir, "--", oldpkg] mempty
        procs "cabal" ["unpack", "-v0", format ("--destdir="%fp) tmpDir, "--", newpkg] mempty
        gcl <- liftIO (guessChangelog (tmpDir </> fromText oldpkg) (tmpDir </> fromText newpkg))
        case gcl of
          Right txt -> liftIO (updateChangesFile opts txt)
          Left desc -> case desc of
            NoCommonChangelogFiles l r  -> undefined
            MoreThanOneChangelogFile p  -> undefined
            UnmodifiedTopIsTooLarge p n -> undefined
            NotJustTopAdditions p       -> undefined

updateChangesFile :: Options -> Text -> IO ()
updateChangesFile (now',changesFile,pkg,vers,email) cl' = do
  now <- maybe (Text.pack . formatTime defaultTimeLocale changeLogDateFormat <$> date) return now'
  txt <- readTextFile changesFile <|> pure ""
  let cl = Text.unlines (map (Text.cons ' ' . Text.cons ' ') (Text.lines (Text.stripEnd (Text.stripStart cl'))))
  writeTextFile changesFile $ format
    ("-------------------------------------------------------------------\n\
     \"%s%" - "%s%"\n\
     \\n\
     \- Add "%wp%" at version "%wp%".\n"%s%"\n"%s)
    now email pkg vers cl txt

--
--   let debug :: MonadIO io => Format (io ()) r -> r
--       debug = eprintf
--
--   oldCLF <- Set.fromList <$> listShell (findChangelogFiles oldDir)
--   debug ("directory "%fp%" contains changelog files: "%fps%"\n") oldDir oldCLF
--   newCLF <- Set.fromList <$> listShell (findChangelogFiles newDir)
--   debug ("directory "%fp%" contains changelog files: "%fps%"\n") newDir newCLF
--   let clf' = oldCLF `Set.intersection` newCLF
--   debug ("common files are: "%fps%"\n") clf'
--   clf <- case Set.toAscList clf' of
--            []    -> die "no common changelog files found"
--            [clf] -> return clf
--            _     -> die (format ("cannot handle multiple changelog files: "%fps) clf')
--   debug ("check differences between "%fp%" file in both directories\n") clf
--
--   old <- Text.lines <$> Text.readFile (Text.unpack (format fp (oldDir </> clf)))
--   new <- Text.lines <$> Text.readFile (Text.unpack (format fp (newDir </> clf)))
--   let (top,diff) = span inBoth (getDiff old new)
--       (add,foot) = span inSecond diff
--       topAddOnly = all inBoth foot
--   debug ("top "%d%" lines are not modified\n") (length top)
--   unless (length top < 10) $
--     die (format (fp%" contains more than 10 lines before the first addition") clf)
--   unless topAddOnly $
--     die (format (fp%" contains more than just additions at the top") clf)
--   mapM_ Text.putStrLn (map unDiff add)
--
-- inBoth :: Diff a -> Bool
-- inBoth (Both _ _)   = True
-- inBoth _            = False
--
-- -- inFirst :: Diff a -> Bool
-- -- inFirst (First _)   = True
-- -- inFirst _           = False
--
-- inSecond :: Diff a -> Bool
-- inSecond (Second _) = True
-- inSecond _          = False
--
-- unDiff :: Diff a -> a
-- unDiff (First txt)  = txt
-- unDiff (Both txt _) = txt
-- unDiff (Second txt) = txt
--
-- -- legalEdit :: (Range,Char,Range) -> Bool
-- -- legalEdit (left,'a',right) = True
-- -- legalEdit _                = False
--
-- -- type LineNumber = Word
-- -- type Length = Word
-- -- type Range = (LineNumber, Length)
--
-- -- rangePattern :: Pattern Range
-- -- rangePattern = do
-- --   start <- decimal
-- --   end   <- fromMaybe (start+1) <$> optional (char ',' *> decimal)
-- --   when (end <= start) (fail "end is smaller than beginning of range")
-- --   return (start,end-start)
--
-- -- hunkSpecPattern :: Pattern (Range,Char,Range)
-- -- hunkSpecPattern = do
-- --   left  <- rangePattern
-- --   ty    <- choice [oneOf "acd"]
-- --   right <- rangePattern
-- --   pure (left,ty,right)
--
-- findChangelogFiles :: FilePath -> Shell FilePath
-- findChangelogFiles dirPath =
--   onFiles (grepText changelogFilePattern) (filename <$> ls dirPath)
--
-- changelogFilePattern :: Pattern Text
-- changelogFilePattern = star dot <> asciiCI "change" <> star dot
--
-- -- * Utility Functions
--
-- listShell :: MonadIO io => Shell a -> io [a]
-- listShell = flip fold Fold.list

argParsec :: Parsec a => ArgName -> Optional HelpMessage -> Parser a
argParsec argname help = arg (simpleParsec . Text.unpack) argname help

wp :: Pretty a => Format r (a -> r)
wp = makeFormat (Text.pack . prettyShow)

wpl :: Pretty a => Format r ([a] -> r)
wpl = makeFormat (Text.intercalate ", " . map (Text.pack . prettyShow))

-- fps :: Format r (Set FilePath -> r)
-- fps = makeFormat (Text.intercalate ", " . map (format fp) . Set.toAscList)

-- runTest :: IO ()
-- runTest = withArgs ["mtl.changes", "mtl", "2.2"] main

debug :: MonadIO io => Format (io ()) r -> r
debug = eprintf

-- | Appropriate format parameter for 'formatTime' and 'parseTimeM'.
changeLogDateFormat :: String
changeLogDateFormat = "%a %b %e %H:%M:%S %Z %Y"
