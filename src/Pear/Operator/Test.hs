module Pear.Operator.Test where

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

algebra = PAlgebra binary_lists unary_lists [intelel]

emptyStack = AStack [(Par Open)] []

lparse = parse (runReaderT (execStateT (parseAll) emptyStack)  algebra) ""

toTokens :: String -> Either ParseError [AToken LExp]
toTokens s = let foo = lparse s
             in case foo of
               Left a -> Left a
               Right b -> Right $ outStack b


toTree :: Either ParseError [AToken LExp] -> Either ParseError (AST LExp (AToken LExp))
toTree (Left a) = Left a
toTree (Right tokens) = Right ((head . snd) $ execState (buildTree) (tokens, []))

evaled :: Either ParseError (AST LExp (AToken LExp)) -> Either ParseError LExp
evaled (Left a) = Left a
evaled (Right b) = Right  (eval b)

-- use run everthing to parse and evaluate any expression
-- as an LExpression

runEverything :: String -> Either ParseError LExp
runEverything = evaled . toTree . toTokens

