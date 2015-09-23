module Pear.Operator (pearAlgebra) where

import Pear.Types

import Pear.Operator.Lexer (PAlgebra(..))
import Pear.Operator.Concrete
-- import Pear.Operator.Algebra
-- import Pear.Operator.Stack
-- import Pear.Operator.Lexer
-- import Pear.Operator.Tree

import Text.Parsec.String (Parser)

algebra :: PAlgebra Ast
algebra = PAlgebra binaryLists unaryLists [integerConstant]

pearAlgebra :: Parser Ast
pearAlgebra = parseAlgebra algebra 
