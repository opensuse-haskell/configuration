module ParseStackageConfig where

import Control.Applicative
import Data.Functor
import Distribution.Compat.CharParsing
import Distribution.Package
import Distribution.Parsec
import Distribution.Parsec.FieldLineStream
import Distribution.Version

stackageConfig :: ParsecParser [Dependency]
stackageConfig = many comment *> constraints

constraints :: ParsecParser [Dependency]
constraints = string "constraints:" *> sepBy (token constraint) (char ',')

constraint :: ParsecParser Dependency
constraint = try installed <|> parsec

installed :: ParsecParser Dependency
installed = Dependency <$> parsec <*> (token (string "installed") $> noVersion) <*> pure mempty

token :: CharParsing p => p a -> p a
token p = spaces *> p

comment :: ParsecParser ()
comment = skipOptional (string "--" *> many (satisfy (/= '\n'))) <* (void (char '\n') <|> eof)

parse :: MonadFail m => ParsecParser a -> String -> m a
parse p x = either (fail . show) return (runParsecParser p "" (fieldLineStreamFromString x))

-- input :: IO String
-- input = readFile "/home/simons/src/opensuse-haskell/_build/cabal-lts-11.config"

-- foo :: IO [Dependency]
-- foo = input >>= parse stackageConfig
