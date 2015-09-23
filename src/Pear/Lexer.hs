module Pear.Lexer where

import Prelude hiding (lex)

import Pear.Operator
import Pear.Types

import Text.Parsec
import Text.Parsec.String

parseEverything :: Parser Ast
parseEverything = pearAlgebra

lex = parse (parseEverything) ""
