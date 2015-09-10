-- Integer.hs

module Pear.Integer where

import Pear.Operator
import Text.Parsec
import Text.Parsec.String (Parser)
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

-- spaces = many $ oneOf " \n\t\r"

lexeme :: Parser a -> Parser a
lexeme a = spaces *> a <* spaces

tokn :: String -> b -> Parser b
tokn a b = (lexeme . string $ a) >> return b

integer :: Parser Int
integer = lexeme (read <$> many1 (satisfy (isDigit)))

plus, minus, times, divide, exponnt :: Binary LExp
plus = Binary (tokn "+" (BinaryExp Plus)) 1 L
minus = Binary (tokn "-" (BinaryExp Minus)) 1 L
times = Binary (tokn "*" (BinaryExp Times)) 2 L
divide = Binary (tokn "/" (BinaryExp Divide)) 2 L
exponnt = Binary (tokn "^" (BinaryExp Exponent)) 3 L

address, negative :: Unary LExp
address = Unary (tokn "&" (UnaryExp Address))
negative = Unary (tokn "-" (UnaryExp Negative))

binary_lists = groupBy f [plus, minus, times, divide, exponnt] where
  f a b = precedence a == precedence b
unary_lists = [address, negative]
intelel = Const . LInt <$> integer

algebra = Algebra binary_lists unary_lists [intelel]

lparse :: String -> Either ParseError LExp
lparse = parse (expression algebra algebra) ""
