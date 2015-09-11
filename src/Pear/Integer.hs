{-# LANGUAGE RankNTypes #-}

-- Integer.hs


module Pear.Integer where

import Pear.Operator
import Text.Parsec
-- import Text.Parsec.String (Parser)
import Control.Monad.Reader
import Data.Char (isDigit)
import Data.List (groupBy)

newtype LInt = LInt Int deriving (Show)

data LBinary 
  = Plus 
  | Minus
  | Times
  | Divide
  | Exponent
  deriving (Show)

data LUnary 
  = Address
  | Negative
  deriving (Show)

data LExp 
  = BinaryExp LBinary LExp LExp
  | UnaryExp LUnary LExp 
  | Const LInt
  deriving (Show)

type LParser a = forall u. Parsec String u a

lexeme :: LParser a -> LParser a
lexeme a = spaces *> a <* spaces

tokn :: String -> b -> LParser b
tokn a b = (lexeme . string $ a) >> return b

integer :: LParser a
integer = lexeme (read <$> many1 (satisfy (isDigit)))

-- algebra :: Algebra Int
-- algebra = a where
--   a = Algebra 0 [b] [] [integer] 
--   b = Binary op L
--   op = putState a >> tokn "+" (BinaryExp Plus)
  

-- plus :: Binary LExp
-- plus = Binary (tokn "+" (BinaryExp Plus)) L

-- lparse :: String -> Either ParseError LExp
-- lparse = parse (expression algebra algebra) ""

--algebra = Algebra binary_lists unary_lists [intelel]






























































-- plus, minus, times, divide, exponnt :: Binary LExp
-- minus = Binary (tokn "-" (BinaryExp Minus)) 1 L
-- times = Binary (tokn "*" (BinaryExp Times)) 2 L
-- divide = Binary (tokn "/" (BinaryExp Divide)) 2 L
-- exponnt = Binary (tokn "^" (BinaryExp Exponent)) 3 L

-- address, negative :: Unary LExp
-- address = tokn "&" (UnaryExp Address)
-- negative =tokn "-" (UnaryExp Negative)

-- binary_lists = groupBy f [plus, minus, times, divide, exponnt] where
--   f a b = precedence a == precedence b
-- unary_lists = [address, negative]
-- intelel = Const . LInt <$> integer

