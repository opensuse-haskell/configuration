module ParseUtils ( parseText ) where

import Distribution.Parsec

parseText :: (Parsec a, MonadFail m) => String -> String -> m a
parseText errM buf =
  maybe (fail ("invalid " ++ errM ++ ": " ++ show buf))
        return
        (simpleParsec buf)
