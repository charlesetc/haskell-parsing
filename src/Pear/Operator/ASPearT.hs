module Pear.Operator.ASPearT where

import Pear.Operator.Algebra

import Control.Monad.State.Lazy
import Data.Functor


data AST a b = UNode { uNode :: b, child :: AST a b}
            | BNode { bNode :: b, lChild :: AST a b, rchild :: AST a b}
            | Leaf { terminal :: b } deriving (Show)


-- Build an abstract syntax tree of arithmetic tokens
-- I would really like to refactor this to get rid of this tuple
-- state and use transformers or anything else. 

type PearTree a = State ([AToken a], [AST a (AToken a)])

growTree :: PearTree a (AST a (AToken a))
growTree = do
  toks <- fst <$> get
  case toks of
    [] -> error "unbalanced expression"
    _ -> case (head toks) of
      (Sym _) -> buildLeaf
      (Bin _) -> buildBNode
      (Un _) -> buildUNode

buildLeaf :: PearTree a (AST a (AToken a))
buildLeaf = do
  leaf <- head . fst <$> get
  modify (\(tks,trs) -> ((tail tks), (Leaf leaf):trs))
  return (Leaf leaf)


buildBNode :: PearTree a (AST a (AToken a))
buildBNode = do
  node <- head . fst <$> get
  modify (\(tks, trs) -> (tail tks, trs))
  child2 <- growTree
  child1 <- growTree
  let bNode = BNode node child1 child2
  modify (\(tks, trs) -> (tks, bNode:trs))
  return bNode

buildUNode :: PearTree a (AST a (AToken a))
buildUNode =  do
  node <- head . fst <$> get
  modify (\(tks, trs) -> (tail tks, trs))
  child <- growTree
  let uNode = UNode node child
  modify (\(tks, trs) -> (tks, uNode:trs))
  return uNode

buildTree :: [AToken a] -> AST a (AToken a)
buildTree tokens = (head . snd) $ execState (growTree) (tokens, [])

evalTree :: AST a (AToken a) -> a
evalTree t = case t of
  UNode (Un (Unary uop)) c -> uop (evalTree c)
  BNode (Bin (Binary bop _ _)) c1 c2 -> bop (evalTree c1) (evalTree c2)
  Leaf (Sym a) -> a
