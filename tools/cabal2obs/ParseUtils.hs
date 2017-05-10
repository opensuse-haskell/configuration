module ParseUtils where

import Development.Shake
import Distribution.Package
import Distribution.Text
import System.IO.Error
import Data.Char

readConfigFile :: FilePath -> Action [String]
readConfigFile p = do
  buf <- readFileLines p
  return [ l | l@(c:_) <- buf, c /= '#' ]

readConstraintList :: FilePath -> Action [Dependency]
readConstraintList p = readConfigFile p >>= \x -> liftIO $  fileErrorContext p (mapM (parseText "constraint") x)

readPackageNameList :: FilePath -> Action [PackageName]
readPackageNameList p = readConfigFile p >>= \x -> liftIO $ fileErrorContext p (mapM (parseText "package name") x)

fileErrorContext :: FilePath -> IO a -> IO a
fileErrorContext p = modifyIOError (\e -> annotateIOError e "" Nothing (Just p))

parseText :: (Text a, Monad m) => String -> String -> m a
parseText errM buf =
  maybe (fail ("invalid " ++ errM ++ ": " ++ show buf))
        return
        (simpleParse buf)

stripSpaces :: String -> String
stripSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace
