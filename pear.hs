 -- pear.hs
module Pear where

import Text.Parsec hiding (notFollowedBy)
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Control.Applicative hiding (many, (<|>))

type Type = String
type Name = String

data Exp = T Type
         | Fun { 
             name :: (Maybe Name),
             par :: (Maybe Exp),
             ret :: Exp
           } deriving (Show, Eq)

-- Alias for simply running a parser.
run :: Parser a -> String -> Either ParseError a
run p = parse p ""

-- Sucks up as much whitespace as possible.
whitespace :: Parser String
whitespace = many $ oneOf " \t\n\r"

-- Removes whitespace on both sides of a parser
lexeme :: Parser String -> Parser String
lexeme p = whitespace *> p <* whitespace

inner_word :: Parser String
inner_word = many $ noneOf " \t\n\r.;:()->"

var :: Parser String
var = lexeme $ do
  a <- letter 
  rest <- inner_word
  return $ a : rest

genus :: Parser Exp
genus = T <$> var

arrow :: Parser String 
arrow = lexeme $ string "->"

(-->) :: Parser Exp -> Parser Exp -> Parser Exp
(-->) first second = do
  domain <- first 
  arrow
  range <- second
  return $ Fun Nothing (Just domain) range

end :: Parser a -> Parser a
end a = do
  x <- a
  eof
  return x

parens :: Parser Exp -> Parser Exp
parens f = (between (string "(") (string ")") f)

-- Leo's masterpiece
function :: Parser Exp -> Parser Exp
function a = try ((atom a) --> (function a))
  <|> try (atom a)

atom :: Parser Exp -> Parser Exp
atom a = try (parens (function a))
  <|> try a 
  
named :: Parser Exp -> Parser Exp
named p = do
  x <- var
  string ":"
  y <- p 
  return $ case y of
    T _ -> Fun (Just x) Nothing y
    Fun _ p r -> Fun (Just x) p r
