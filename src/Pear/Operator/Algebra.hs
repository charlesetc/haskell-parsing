module Pear.Operator.Algebra where

data Associativity = L | R deriving (Show, Eq)

data Binary a = Binary { getBOp :: a -> a -> a
                       , preced :: Int
                       , assoc :: Associativity
                       }
instance Show (Binary a) where
  show = \x -> show "bOP"

newtype Unary a = Unary { getUOp :: a -> a }

instance Show (Unary a) where
  show = \x -> (show "U")

data Paren = Open | Close deriving Show

data AToken a = Bin (Binary a) | Un (Unary a) | Sym a | Par Paren deriving Show

