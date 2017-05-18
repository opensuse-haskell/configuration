module ParseStackageConfig where

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
installed = Dependency <$> parse <*> (token (string "installed") *> pure noVersion)

token :: ReadP r a -> ReadP r a
token p = skipSpaces *> p

comment :: ReadP r String
comment = string "--" *> munch (/= '\n') <* char '\n'

runP :: Monad m => ReadP a a -> String -> m a
runP p str = case [ r | (r,"") <- readP_to_S p str] of
               [r] -> return r
               []  -> fail "no parse"
               _   -> fail "ambiguous parse"
