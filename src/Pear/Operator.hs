module Pear.Operator where

import Pear.Lexer
import Text.Parsec.String (Parser)
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec.Combinator
import Control.Monad.Reader

type LUserParser a b = Parsec 
  String
  (Algebra a)
  b 

data Associativity = L | R 

data Binary a = Binary { operator :: LUserParser a (a -> a -> a)
                       , associativity :: Associativity
                       }

type Unary a = LUserParser a (a -> a)

type Symbol a = LUserParser a a

data Algebra a = Algebra -- precedence is based off the order of the list.
                         { current :: Int
                         , binary :: [Binary a]
                         , unary :: [Unary a]
                         , symbols :: [Symbol a]
                         }

expression :: LUserParser a a
expression = try unary_expression
          <|> atom

atom :: LUserParser a a
atom = do
  algebra <- getState
  try (choice (symbols algebra)) -- <|> (parens (expression))

unary_expression :: LUserParser a a
unary_expression = do 
  algebra <- getState
  op <- choice (unary algebra)
  arg <- expression
  return $ op arg

-- no_operator = parserFail "No operator can be parsed."
-- 
-- atom :: Algebra a -> Parser a
-- atom a = try (choice (symbols a)) <|> (parens (expression a a))
-- 
-- binaries :: Algebra a -> [(Parser (a -> a -> a))]
-- binaries = process . binary where
--   process [] = [no_operator]
--   process a = map bop $ head a
-- 
-- algebra_rest :: Algebra a -> Algebra a
-- algebra_rest a = a { binary = tail . binary $ a } -- need to case in future
-- 
-- next :: Algebra a -> Algebra a
-- next a = a { binary = tail (binary a) }
-- 
-- expression :: Algebra a -> Algebra a -> Parser a
-- expression original current 
--   = try (unary_expression original)
--  <|> atom original
-- 
-- binary_expression :: Algebra a -> Algebra a -> Parser a
-- binary_expression original current 
--    = try (parseE original current) 
--   <|> expression original (algebra_rest current)
-- 
-- 
-- parseE :: Algebra a -> Algebra a -> Parser a
-- parseE original current = do
--   arg1 <- (try $ parseE original (next current)) <|> (atom original)
--   oper <- choice $ binaries current
--   arg2 <- (parseE original current)
--   return $ oper arg1 arg2
-- 
-- -- parseT :: Algebra a -> Algebra a -> Parser a
-- -- parseT original current = do
-- 
-- unary_expression :: Algebra a -> Parser a
-- unary_expression a = do 
--   op <- choice (map uop (unary a))
--   arg <- (expression a a)
--   return $ op arg
