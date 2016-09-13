module ParseUtils where

import Distribution.Text

parseText :: (Text a, Monad m) => String -> String -> m a
parseText errM buf =
  maybe (fail ("invalid " ++ errM ++ ": " ++ show buf))
        return
        (simpleParse buf)
