module Pear.Lexer where

import Prelude hiding (lex)
import Text.Parsec.String
import Pear.Operator.Algebra
import Pear.Operator.Stack
import Pear.Operator.Lexer
import Pear.Operator.Tree

import Pear.Operator.Concrete
import Pear.Types

import System.IO
import Control.Monad.Reader
import Control.Monad.State.Lazy

import Text.Parsec (parse, ParseError)

algebra = PAlgebra binaryLists unaryLists [integerConstant]

parseAlgebra :: PAlgebra a -> Parser (AST a (AToken a))
parseAlgebra alg = buildTree <$> (shYardOutput alg)

parseToExpression :: (PAlgebra a) -> Parser a
parseToExpression alg = evalTree <$> (parseAlgebra alg)

lex = parse (parseToExpression algebra) ""
