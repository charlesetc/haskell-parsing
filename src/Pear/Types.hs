module Pear.Types where

type Name = String

data Value = VInt Int 
       | VString String deriving (Show)

data Ast
  = Const Value
  | Function Name [Ast]
  deriving (Show)
