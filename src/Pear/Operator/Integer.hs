-- Integer.hs

module Pear.Operator.Integer where


import Pear.Operator.Algebra
import Pear.Lexer(reservedOp, identifier)

import Text.Parsec.String (Parser)
import Text.Parsec (many, oneOf, string, many1, parse, ParseError)
import Text.ParserCombinators.Parsec.Char (digit, spaces)



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
  | Const LConst
  deriving (Show)

data LConst = IntLit LInt | Fun B deriving Show

newtype B = B [String] deriving Show

elbl :: Parser B
elbl = B <$> many1 identifier

integer :: Parser Int
integer = lexeme (read <$> many1 (digit))

whitespace = many $ oneOf " \n\r\t"

lexeme :: Parser a -> Parser a
lexeme p = whitespace *> p <* whitespace

plus, minus, times, divide, exponnt :: Parser (Binary LExp)
plus = (reservedOp "+") >> (return $ Binary (BinaryExp Plus) 0 L)
minus = (reservedOp "-") >> (return $ Binary (BinaryExp Minus) 0 L)
times = (reservedOp "*") >> (return $ Binary (BinaryExp Times) 1 L)
divide =  (reservedOp "/") >> (return $ Binary (BinaryExp Divide) 1 L)
exponnt = (reservedOp "^") >> (return $ Binary (BinaryExp Exponent) 2 R)

address, negative :: Parser (Unary LExp)
address = (reservedOp "&") >> (return $ Unary (UnaryExp Address))
negative = (reservedOp "~") >> (return $ Unary (UnaryExp Negative))

binary_lists = [plus, minus, times, divide, exponnt]
unary_lists = [address, negative]

elalal = elbl >>= (return . Const . Fun)
intelel = integer >>= (return . Const . IntLit . LInt)
