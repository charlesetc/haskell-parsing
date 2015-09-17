module Pear.Operator.ASPearT where

import Pear.Operator.Algebra

import Control.Monad.State.Lazy
import Data.Functor


data AST a b = UNode { uNode :: b, child :: AST a b}
            | BNode { bNode :: b, lChild :: AST a b, rchild :: AST a b}
            | Leaf { terminal :: b } deriving (Show)


instance Functor (AST a) where
  fmap f (UNode n c) = UNode (f n) (fmap f c)
  fmap f (BNode n c1 c2) = BNode (f n) (fmap f c1) (fmap f c2)
  fmap f (Leaf l) = Leaf (f l)


eval :: AST a (AToken a) -> a
eval t = case t of
  UNode (Un (Unary uop)) c -> uop (eval c)
  BNode (Bin (Binary bop _ _)) c1 c2 -> bop (eval c1) (eval c2)
  Leaf (Sym a) -> a

-- Build an abstract syntax tree of arithmetic tokens
-- I would really like to refactor this to get rid of this tuple
-- state and use transformers or anything else. 

type PearTree a = State ([AToken a], [AST a (AToken a)])

buildTree :: PearTree a (AST a (AToken a))
buildTree = do
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
  child2 <- buildTree
  child1 <- buildTree
  let bNode = BNode node child1 child2
  modify (\(tks, trs) -> (tks, bNode:trs))
  return bNode

buildUNode :: PearTree a (AST a (AToken a))
buildUNode =  do
  node <- head . fst <$> get
  modify (\(tks, trs) -> (tail tks, trs))
  child <- buildTree
  let uNode = UNode node child
  modify (\(tks, trs) -> (tks, uNode:trs))
  return uNode
