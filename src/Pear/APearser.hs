module Pear.APearser where


import Pear.Algebra
import Pear.AStack
import qualified Pear.ALexer as L

import Control.Monad.Morph
import Control.Monad.State.Lazy

import Text.Parsec.String as P


-- the APearser is essentially state  on top of an ALexer.
-- The outer  monad is just AComp, which is parsing the expression as
-- it takes in tokens.

type APearser a = (AComp a) (L.ALexer a)


--these are lifted parser combinators

-- the type checker didn't like the type I gave it,
-- probably because the function is much more generic.
--hoist2ARS :: (L.ALexer a b -> L.ALexer a b) ->
--             (APearser a b -> APearser a b)
hoist2ARS f = \p q -> (
  StateT $ \s -> ((runStateT p s) `f` (runStateT q s)))

try :: APearser a b -> APearser a b
try = hoist (L.try)

(<|>) = hoist2ARS (L.<|>)



-- these parse strings into tokens then throw them into
-- the outer state for processesing. Note that the end
-- is the way it is becaue the implementation of the parser
-- requires the last input to be a close parens.

end :: APearser a ()
end = do
  (lift L.end) >> pushToken (Par Close)

parseOne :: APearser a ()
parseOne = (lift L.aLexer) >>= pushToken

parseAll :: APearser a ()
parseAll = (try end) <|> (parseOne >> parseAll)
