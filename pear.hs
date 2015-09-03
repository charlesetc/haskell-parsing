 -- pear.hs
module Pear where

import Text.ParserCombinators.ReadP
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
run :: ReadP Exp -> ReadS Exp 
run p = readP_to_S p

-- Removes whitespace on both sides of a parser
lexeme :: ReadP String -> ReadP String
lexeme p = skipSpaces *> p <* skipSpaces 

inner_word :: ReadP String
inner_word = many $ satisfy (`elem` "\t\n\r.;:()->")

var :: ReadP String
var = lexeme $ do
  a <- satisfy $ const True -- letter
  rest <- inner_word
  return $ a : rest

genus :: ReadP Exp
genus = T <$> var

arrow :: ReadP String 
arrow = lexeme $ string "->"

(-->) :: ReadP Exp -> ReadP Exp -> ReadP Exp
(-->) first second = do
  domain <- first 
  arrow
  range <- second
  return $ Fun Nothing (Just domain) range

end :: ReadP a -> ReadP a
end a = do
  x <- a
  eof
  return x

parens :: ReadP Exp -> ReadP Exp
parens f = (between (string "(") (string ")") f)

-- Leo's masterpiece
function :: ReadP Exp -> ReadP Exp
function a = (atom a) --> (function a) +++ atom a

generic = function genus <* eof

atom :: ReadP Exp -> ReadP Exp
atom a = parens (function a) +++ a 
  
named :: ReadP Exp -> ReadP Exp
named p = do
  x <- var
  string ":"
  y <- p 
  return $ case y of
    T _ -> Fun (Just x) Nothing y
    Fun _ p r -> Fun (Just x) p r
