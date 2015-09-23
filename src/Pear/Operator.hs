module Pear.Operator (pearAlgebra) where

import Pear.Types

import Pear.Operator.Lexer (PAlgebra(..), )
import Pear.Operator.Concrete

import Pear.Lexer.Helper (identifier, parens)
import Text.Parsec (many, try, (<|>), choice)
-- import Pear.Operator.Algebra
-- import Pear.Operator.Stack
-- import Pear.Operator.Lexer
-- import Pear.Operator.Tree

import Text.Parsec.String (Parser)

parseSingleFunction :: Parser Ast
parseSingleFunction = (\x -> Function x []) <$> identifier

parseBasic :: Parser Ast
parseBasic = parseSingleFunction 
          <|> choice [integerConstant, parseFunction]

parseArguments :: Parser [Ast]
parseArguments = many $ parseBasic <|> pearAlgebra

parseFunction :: Parser Ast
parseFunction = do
  name <- identifier
  args <- parseArguments
  return $ Function name args

algebra :: PAlgebra Ast
-- algebra = PAlgebra binaryLists unaryLists [integerConstant, parseFunction]
algebra = PAlgebra binaryLists unaryLists [integerConstant, parseFunction]

pearAlgebra :: Parser Ast
pearAlgebra = parseAlgebra algebra 
