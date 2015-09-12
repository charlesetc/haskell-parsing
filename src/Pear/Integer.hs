-- Integer.hs

module Pear.Integer where

import Pear.Operator
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad.Reader
import Control.Monad.State.Lazy
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

lexeme :: Parser a -> Parser a
lexeme a = spaces *> a <* spaces

tokn :: String -> a -> Parser a
tokn a b = (lexeme . string $ a) >> return b

integer :: Read a => Parser a
integer = lexeme (read <$> many1 (satisfy (isDigit)))

plus :: Parser LExp
plus = Parser (tokn "+" (BinaryExp Plus)) L

newAlgebra :: Algebra LExp
newAlgebra bs us ss = Algebra 
  (lift $ lift $ bs)
  (lift $ lift $ us)
  (lift $ lift $ ss)

algebra = newAlgebra [plus] [] [integer]

aparse = parse (runStateT (runReaderT expression algebra) 0) ""



-- lparse :: String -> Either ParseError LExp
-- lparse = parse (expression algebra algebra) ""











































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

