module Pear.Operator where

import Pear.Operator.Algebra
import Pear.Operator.Stack
import Pear.Operator.Lexer
import Pear.Operator.Tree

import Text.Parsec.String (Parser)

shuntingYard :: PAlgebra a -> Parser (AST a (AToken a))
shuntingYard alg = buildTree <$> (shYardOutput alg)

parseAlgebra :: (PAlgebra a) -> Parser a
parseAlgebra alg = evalTree <$> (shuntingYard alg)
