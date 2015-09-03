-- parens.hs

import Text.Parsec hiding (notFollowedBy)
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Control.Applicative hiding (many, (<|>))

run :: Parser a -> String -> Either ParseError a
run p s = parse p "" s

open :: Parser String
open = (const "") <$> string "("
close :: Parser String
close = (const "") <$> string ")"

endstring :: Parser String
endstring = (const "") <$> eof

balance :: Parser String
balance = try (balance >> balance >> endstring)
  <|> try (open >> close >> endstring)
  <|> try (open >> balance >> close >> endstring)
  <|> try (open >> close)
  <|> try (open >> balance >> close)
  <|> try (balance >> balance)
