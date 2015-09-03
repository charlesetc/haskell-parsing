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

basic_type :: Parser Exp
basic_type = T <$> var

arrow :: Parser String 
arrow = lexeme $ string "->"

arrow_expression :: Parser Exp -> Parser Exp
arrow_expression f = do
  domain <- basic_type 
  arrow
  range <- f
  return $ Fun Nothing (Just domain) range

no_arrow :: Parser String
no_arrow = lexeme $ do {nothyphen <|> end; notbracket <|> end} where
  nothyphen = (\x -> x : []) <$> (satisfy (\x -> x /= '-'))
  notbracket = (\x -> x : []) <$> (satisfy (\x -> x /= '>'))
  end = try (const "" <$> lookAhead eof)


function :: Parser Exp
-- function = choice [ between (string "(") (string ")") function
--                   , basic_type <* no_arrow
--                   , arrow_expression function
--                   ]
function = (try (basic_type <* no_arrow)) <|>
           (try (arrow_expression function)) <|>
           (try (between (string "(") (string ")") function))

named :: Parser Exp -> Parser Exp
named p = do
  x <- var
  string ":"
  y <- p 
  return $ case y of
    T _ -> Fun (Just x) Nothing y
    Fun _ p r -> Fun (Just x) p r


