module ParseStackageConfig where

import MyCabal

import Control.Applicative
import Data.Functor

stackageConfig :: ParsecParser [Dependency]
stackageConfig = many comment *> constraints

constraints :: ParsecParser [Dependency]
constraints = string "constraints:" *> sepBy (token constraint) (char ',')

constraint :: ParsecParser Dependency
constraint = try installed <|> parsec

installed :: ParsecParser Dependency
installed = Dependency <$> parsec <*> (token (string "installed") $> noVersion) <*> pure undefined -- TODO: generate empty Dependency type

token :: CharParsing p => p a -> p a
token p = spaces *> p

comment :: ParsecParser ()
comment = skipOptional (string "--" *> many (satisfy (/= '\n'))) <* (void (char '\n') <|> eof)

parse :: MonadFail m => ParsecParser a -> String -> m a
parse p x = either (fail . show) pure (runParsecParser p "" (fieldLineStreamFromString x))

-- input :: IO String
-- input = readFile "/home/simons/src/opensuse-haskell/_build/cabal-lts-11.config"

-- foo :: IO [Dependency]
-- foo = input >>= parse stackageConfig
