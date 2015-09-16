module Pear.AStack where

import Data.Functor
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Control.Applicative
import Pear.Algebra


-- this is an implementation of the "shunting yard algorithm" for parsing
-- binary and unary operators on some set of  symbols
-- with precedence and parenthesis
-- symbols

data AStack a = AStack { opStack :: [AToken a]
                       , outStack :: [AToken a]
                       }

instance (Show a) => Show (AStack a) where
  show a = show (show $ outStack a, show $ length $ opStack a)

type AComp a = StateT (AStack a)

-- This sorts the incoming token into one of the two stacks, "open parens"
-- and ops into the opStack and symbols to the outStack. "Close parens"
-- flush the stack until the previous open parens is reached.

pushToken :: (Monad m) => (AToken a) -> AComp a m ()
pushToken a =
  case a of
    Par Close -> flush
    Par Open -> pushOpen
    Bin _ -> pushBOp a
    Un _ -> pushUOp a
    _ -> modify (simpleOutPush a)
  where
    simpleOutPush a s = s { outStack = a:(outStack s) }

-- need this rather than assigning parens a default precedence order
pushOpen :: (Monad m) => AComp a m ()
pushOpen = modify (\s -> s { opStack = (Par Open):(opStack s)} )

-- pushes a unary op to stack
pushUOp :: (Monad m) => (AToken a) -> AComp a m ()
pushUOp uop = modify (\s -> s { opStack = uop:(opStack s)} )

--pops the top of the opStack
popOp :: (Monad m) => AComp a m (AToken a)
popOp = do
  ops <- opStack <$> get
  modify (putOps $ tail ops)
  return $ head ops
  where
      putOps ops s = s { opStack = ops }

-- reads the operator level of the top operator, parens given the lowest
-- order of -1
getLevel :: (Monad m) => AComp a m Int
getLevel = do
  ops <- opStack <$> get
  case (head ops) of
    (Par Open) -> return (-1)
    (Un _) -> return (-1)
    (Bin b) -> return $ preced b

-- push the incoming op. if it has "appropriately high precedence",
-- which is associativity-dependent, we will need to pop off the
-- op stack some before pushing, otherwise just push it.
pushBOp :: (Monad m) => (AToken a) -> AComp a m ()
pushBOp b@(Bin bop) = do
  currentOpLevel <- getLevel
  let as = assoc bop
  case as of
    L -> if (currentOpLevel >=  preced bop)
         then reducePush b
         else modify (simplePush b)
    R -> if (currentOpLevel >  preced bop)
         then reducePush b
         else modify (simplePush b)
  where
    reducePush b = reduce >> (pushBOp b)
    simplePush b s = s { opStack = b:(opStack s) }

-- called to move an operator from the opStack to the outStack
reduce :: (Monad m) => AComp a m ()
reduce = popOp >>= (\op ->  modify (\s -> s { outStack = op:(outStack s) }) )

-- reduce all the way down to the most recent open parenthesis
flush :: (Monad m) => AComp a m ()
flush = do
  ops <- opStack <$> get
  case (head ops) of
    (Par Open) -> modify ( \s -> s { opStack = tail ops } )
    _ -> reduce >> flush


