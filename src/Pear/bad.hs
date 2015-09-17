-- This is version 2:

import Pear.Lexer
--import Pear.Integer
import Text.Parsec
import Text.Parsec.String (Parser)


data Binary a = Binary { op :: Parser (a -> a-> a)
                       , assoc :: Associativity
                       }

data Associativity = L | R deriving (Show, Eq)

type Unary a = Parser (a -> a)

type Symbol a = Parser a

data Algebra a = Algebra { binaries :: [Binary a]
                         , unaries :: [Unary a]
                         , symbols :: [Symbol a]
                         }

expression :: Algebra a -> Algebra a -> Parser a
expression curr orig = try (unExpression curr orig)
                     <|> try (binExpression curr orig)
                     <|> atom curr orig

unExpression :: Algebra a -> Algebra a -> Parser a
unExpression curr orig = do
  op <- (choice . unaries) orig
  arg <- expression orig orig
  return $ op arg

binExpression :: Algebra a -> Algebra a -> Parser a
binExpression curr orig = case (binaries curr) of
  [] -> parserZero
  otherwise -> case (assoc . head . binaries) curr of
    L -> leftBinary curr orig
    R -> rightBinary curr orig

leftBinary :: Algebra a -> Algebra a -> Parser a
leftBinary curr orig = try applyOpL
                    <|> expression nextAlg orig
  where
    nextAlg = curr { binaries = (tail . binaries) curr }
    applyOpL = do
      arg1 <- expression nextAlg orig
      op <- (op . head . binaries) curr
      arg2 <- expression curr orig
      return $ op arg1 arg2

rightBinary :: Algebra a -> Algebra a -> Parser a
rightBinary curr orig = (try ((expression nextAlg orig) >>= (doA curr orig)))
                      <|> (expression nextAlg orig)
  where
    nextAlg = curr { binaries = (tail . binaries) curr }
    doA curr orig arg1 = do
      op <- (op . head . binaries) curr
      arg2 <- try ((expression nextAlg orig) >>= (doA curr orig)) <|> (expression nextAlg orig)
      return $ op arg1 arg2

atom curr orig = ((choice . symbols) orig) <|> (parens $ expression orig orig)


newtype LInt = LInt Integer deriving (Show)

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


plus, minus, times, divide, exponnt :: Parser (LExp -> LExp -> LExp)
plus = (reservedOp "+") >> (return $ BinaryExp Plus)
minus = (reservedOp "-") >> (return $ BinaryExp Minus)
times = (reservedOp "*") >> (return $ BinaryExp Times)
divide = (reservedOp "/") >> (return $ BinaryExp Divide)
exponnt = (reservedOp "^") >> (return $ BinaryExp Exponent)

neg = (reservedOp "-") >> (return $ UnaryExp Negative)
addr = (reservedOp "&") >> (return $ UnaryExp Address)

intelel = integer >>= return . Const . LInt

bs = [Binary plus L, Binary minus L, Binary times L, Binary divide L, Binary exponnt R]
us = [neg, addr]
sys = [intelel]

algebra = Algebra bs us sys

lparse = parse (expression algebra algebra) ""
