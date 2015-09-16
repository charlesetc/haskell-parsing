-- This is version 2:

import Pear.Lexer
--import Pear.Integer
import Text.Parsec
import Text.Parsec.String (Parser)


data Binary a = Binary {
  op :: Parser (a->a->a)
  , associativity :: Associativity
}

data Associativity = L | R

type Symbol a = Parser a

data Algebra a = Algebra {
  binaries :: [Binary a]
  , symbols :: [Symbol a]
}

expression :: Algebra a -> Parser a
expression a = stateful_expression a 0

stateful_expression :: Algebra a -> Int -> Parser a
stateful_expression a i = parse_binary a i <|> atom a

atom :: Algebra a -> Parser a
atom a = try (choice . symbols $ a) <|> parens (expression a)

simple_binary :: Algebra a -> Int -> Parser a
simple_binary a i = do
  arg1 <- (atom a)
  op <- getop a i
  arg2 <- stateful_expression a i
  return $ op arg1 arg2

-- Sorry about this long function...
-- Split it up if you want
parse_binary :: Algebra a -> Int -> Parser a
parse_binary a i
  | i == ((length $ binaries a) - 1) = simple_binary a i
  | otherwise = inner_parse <|> parse_binary a (i + 1)
    where
      inner_parse = do
        arg1 <- parse_binary a (i + 1)
        op <- getop a i
        arg2 <- parse_binary a i
        return $ op arg1 arg2

getop :: Algebra a -> Int -> Parser (a->a->a)
getop a i
  | (length . binaries $ a) > i = op $ (binaries a) !! i
  | otherwise = parserZero



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


plus, minus, times, divide, exponnt :: [Binary LExp]
plus = (reservedOp "+") >> (return $ Binary (BinaryExp Plus) L)
minus = (reservedOp "-") >> (return $ Binary (BinaryExp Minus) L)
times = (reservedOp "*") >> (return $ Binary (BinaryExp Times) L)
divide = (reservedOp "/") >> (return $ Binary (BinaryExp Divide) L)
exponnt = (reservedOp "^") >> (return $ Binary (BinaryExp Exponent) R)


