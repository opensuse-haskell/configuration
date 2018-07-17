module ParseStackageConfig where

import Data.Functor
import Distribution.Compat.ReadP
import Distribution.Package
import Distribution.Text
import Distribution.Version

stackageConfig :: ReadP r [Dependency]
stackageConfig = many comment *> constraints

constraints :: ReadP r [Dependency]
constraints = string "constraints:" *> sepBy (token constraint) (char ',')

constraint :: ReadP r Dependency
constraint = parse +++ installed

installed :: ReadP r Dependency
installed = Dependency <$> parse <*> (token (string "installed") $> noVersion)

token :: ReadP r a -> ReadP r a
token p = skipSpaces *> p

comment :: ReadP r ()
comment = optional (string "--" *> munch (/= '\n')) <* char '\n'

runP :: Monad m => ReadP a a -> String -> m a
runP p str = case [ r | (r,"") <- readP_to_S p str] of
               [r] -> return r
               []  -> fail "no parse"
               _   -> fail "ambiguous parse"


-- import Control.Applicative
-- import Data.Functor
-- import Distribution.Compat.CharParsing
-- import Distribution.Package
-- import Distribution.Parsec.Class
-- import Distribution.Parsec.FieldLineStream
-- import Distribution.Version

-- stackageConfig :: ParsecParser [Dependency]
-- stackageConfig = many comment *> constraints

-- constraints :: ParsecParser [Dependency]
-- constraints = string "constraints:" *> sepBy (token constraint) (char ',')

-- constraint :: ParsecParser Dependency
-- constraint = try installed <|> parsec

-- installed :: ParsecParser Dependency
-- installed = Dependency <$> parsec <*> (token (string "installed") $> noVersion)

-- token :: CharParsing p => p a -> p a
-- token p = spaces *> p

-- comment :: ParsecParser ()
-- comment = skipOptional (string "--" *> (many (satisfy (/= '\n')))) <* (void (char '\n') <|> eof)

-- parse :: Monad m => ParsecParser a -> String -> m a
-- parse p x = either (fail . show) return (runParsecParser p "" (fieldLineStreamFromString x))

-- input :: IO String
-- input = readFile "/home/simons/src/opensuse-haskell/_build/cabal-lts-11.config"

-- foo :: IO [Dependency]
-- foo = input >>= parse stackageConfig
