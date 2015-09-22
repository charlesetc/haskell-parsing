-- Integer.hs

module Pear.Operator.Integer where


import Pear.Operator.Algebra
import Pear.Lexer(reservedOp, identifier, whiteSpace)

import Text.Parsec.String (Parser)
import Text.Parsec (many, oneOf, string, many1, parse, ParseError)
import Text.ParserCombinators.Parsec.Char (digit, spaces)

type Name = String

data Value = VInt Int 
       | VString String deriving (Show)

data Ast
  = Const Value
  | Function Name [Ast]
  deriving (Show)

integer :: Parser Int
integer = lexeme (read <$> many1 (digit))

-- This is not working how I want it to
-- whitespace = many . choice $ [string " ", string "\t", string "\\\n"]

lexeme :: Parser a -> Parser a
lexeme p = whiteSpace *> p <* whiteSpace

binary_function :: String -> Ast -> Ast -> Ast
binary_function name a b = Function name [a, b]
unary_function :: String -> Ast -> Ast
unary_function name a = Function name [a]

plus, minus, times, divide, exponnt :: Parser (Binary Ast)
plus = (reservedOp "+") >> (return $ Binary (binary_function "+") 0 L)
minus = (reservedOp "-") >> (return $ Binary (binary_function "-") 0 L)
times = (reservedOp "*") >> (return $ Binary (binary_function "*") 1 L)
divide = (reservedOp "/") >> (return $ Binary (binary_function "/") 1 L)
exponnt = (reservedOp "^") >> (return $ Binary (binary_function "^") 2 R)

address, negative :: Parser (Unary Ast)
address = (reservedOp "&") >> (return $ Unary (unary_function "&"))
negative = (reservedOp "~") >> (return $ Unary (unary_function "~"))

binary_lists = [exponnt, times, divide, plus, minus]
unary_lists = [address, negative]

-- elalal = elbl >>= (return . Const . Fun)
-- intelel = integer >>= (return . Const . IntLit . LInt)
intelel = Const . VInt <$> integer
