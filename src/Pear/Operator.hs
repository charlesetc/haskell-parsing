-- Operator.js

module Pear.Operator where

import Pear.Lexer
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Combinator -- maybe unecessary
-- import Control.Monad.Reader
import Control.Monad.Trans.Reader
import Control.Monad.State.Lazy


-- Algebraic Pearser
type APearser a b = ReaderT (Algebra a) (StateT Int Parser) b

type Symbol a = APearser a a

type Unary a = APearser a (a -> a)

data Binary a = Binary { operator :: APearser a (a -> a -> a)
                       , associativity :: Associativity
                       }

data Algebra a = Algebra { binaryOperators :: [Binary a]
                         , unaryOperators :: [Unary a]
                         , symbols :: [Symbol a]
                         }

data Associativity = L | R 

expression :: APearser a a
expression = atom

liftPear :: Parser a -> APearser a a
liftPear p = ReaderT $ \_ -> (state (\s -> (p, s)))

-- put >> return p

-- expression = choice
--   [ binaryExpression
--   , unaryExpression
--   , atom 
--   ]

symbolParser :: Algebra a -> APearser a a
symbolParser = liftPear choice . symbols

atom :: APearser a a
atom = do
  a <- ask
  try (symbolParser a) <|> (parens expression)

-- unaryParser :: Algebra a -> APearser a (a -> a)
-- unaryParser = choice . unaryOperators
-- 
-- binaryParser :: Algebra a -> Int -> APearser a (a -> a -> a)
-- binaryParser a n = (binaryOperators a) !! n


-- unaryExpression :: APearser a a
-- unaryExpression = do 
--   a <- ask
--   uop <- unaryParser a
--   exp <- expression
--   return $ uop exp
-- 
-- decrement, increment :: APearser a ()
-- decrement = modify pred
-- increment = modify succ
-- 
-- binaryExpression :: APearser a a
-- binaryExpression = do 
--   a      <- ask
--             decrement
--   exp1   <- expression
--             increment
--   i      <- get
--   binop  <- binaryParser a i
--   exp2   <- expression
--   return $ binop exp1 exp2
