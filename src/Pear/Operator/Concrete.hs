-- concrete.hs

module Pear.Operator.Concrete where

import Pear.Types
import Pear.Operator.Lexer (PAlgebra(..), shYardOutput)
import Pear.Operator.Algebra
import Pear.Operator.Tree
import Pear.Lexer.Helper (reservedOp, identifier, whiteSpace)

import Text.Parsec.String (Parser)
import Text.Parsec (many, oneOf, string, many1, parse, ParseError)
import Text.ParserCombinators.Parsec.Char (digit, spaces)

integer :: Parser Int
integer = lexeme (read <$> many1 (digit))

lexeme :: Parser a -> Parser a
lexeme p = whiteSpace *> p <* whiteSpace

binaryfunction :: String -> Ast -> Ast -> Ast
binaryfunction name a b = Function name [a, b]
unaryfunction :: String -> Ast -> Ast
unaryfunction name a = Function name [a]

plus, minus, times, divide, exponnt :: Parser (Binary Ast)
plus = 
  (reservedOp "+") >> (return $ Binary (binaryfunction "+") 0 L)
minus = 
  (reservedOp "-") >> (return $ Binary (binaryfunction "-") 0 L)
times = 
  (reservedOp "*") >> (return $ Binary (binaryfunction "*") 1 L)
divide = 
  (reservedOp "/") >> (return $ Binary (binaryfunction "/") 1 L)
exponnt = 
  (reservedOp "^") >> (return $ Binary (binaryfunction "^") 2 R)

address, negative :: Parser (Unary Ast)
address = 
  (reservedOp "&") >> (return $ Unary (unaryfunction "&"))
negative = 
  (reservedOp "~") >> (return $ Unary (unaryfunction "~"))

binaryLists = [exponnt, times, divide, plus, minus]
unaryLists = [address, negative]

integerConstant = Const . VInt <$> integer

shuntingYard :: PAlgebra a -> Parser (AST a (AToken a))
shuntingYard alg = buildTree <$> (shYardOutput alg)

parseAlgebra :: (PAlgebra a) -> Parser a
parseAlgebra alg = evalTree <$> (shuntingYard alg)
