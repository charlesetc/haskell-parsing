module Pear.Lexer where

import Prelude hiding (lex)
import Text.Parsec.String (Parser)

import Pear.Operator
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

parseEverything = pearAlgebra
lex = parse (parseEverything) ""
