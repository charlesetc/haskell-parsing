module Pear.Operator where

import Pear.Lexer
import Text.Parsec.String (Parser)
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec.Combinator

data Associativity = L | R 

data Binary a = Binary { bop :: Parser (a -> a -> a)
                       , precedence :: Int
                       , associativity :: Associativity
                       }
data Unary a = Unary { uop :: Parser (a -> a) }

type Symbol = Parser

data Algebra a = Algebra { binary :: [[(Binary a)]]
                         , unary :: [(Unary a)]
                         , symbols :: [(Symbol a)]
                         }

-- no_operator :: Parser a -- this is wrong
no_operator = parserFail "No operator can be parsed."
--
--other Left = Right
--other Right = Left

binaries :: Algebra a -> [(Parser (a -> a -> a))]
binaries = process . binary where
  process [] = [no_operator]
  process a = map bop $ head a

algebra_rest :: Algebra a -> Algebra a
algebra_rest a = a { binary = tail . binaries a } -- need to case in future

next :: Algebra a -> Algebra a
next a = a { binary = tail (binary a) }
 
expression :: Algebra a -> Algebra a -> Parser a
expression original current 
  = try (binary_expression original current)
 <|> try (unary_expression original)
 <|> atom original


binary_expression :: Algebra a -> Algebra a -> Parser a
binary_expression original current 
   = try (parseE original current) 
  <|> expression original (algebra_rest current)


parseE :: Algebra a -> Algebra a -> Parser a
parseE original current = do
  arg1 <- (try $ parseE original (next current)) <|> (atom original)
  oper <- choice $ binaries current
  arg2 <- (parseE original current)
  return $ oper arg1 arg2

-- parseT :: Algebra a -> Algebra a -> Parser a
-- parseT original current = do

atom :: Algebra a -> Parser a
atom a = try (choice (symbols a)) <|> (parens (expression a))

unary_expression :: Algebra a -> Parser a
unary_expression a = do 
  op <- choice (map uop (unary a))
  arg <- (expression a)
  return $ op arg



