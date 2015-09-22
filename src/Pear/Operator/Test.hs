module Pear.Operator.Test where

import Text.Parsec.String
import Pear.Operator.Algebra
import Pear.Operator.AStack
import Pear.Operator.ALexer
import Pear.Operator.APearser
import Pear.Operator.ASPearT

import Pear.Operator.Integer

import System.IO
import Control.Monad.Reader
import Control.Monad.State.Lazy

import Text.Parsec (parse, ParseError)

algebra = PAlgebra binary_lists unary_lists [intelel] -- , elalal

parseAlgebra :: PAlgebra a -> Parser (AST a (AToken a))
parseAlgebra alg = buildTree <$> (shYardOutput alg)

-- use run everthing to parse and evaluate any expression
-- as an LExpression


parseToExpression :: (PAlgebra a) -> Parser a
parseToExpression alg = evalTree <$> (parseAlgebra alg)

lparse = parse (parseToExpression algebra) ""
