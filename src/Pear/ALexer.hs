module Pear.ALexer  where

import Control.Monad.Reader
import Pear.AStack
import Pear.Algebra
import Text.Parsec.Char
import Control.Monad.Morph
import qualified Text.Parsec as P
import qualified Text.Parsec.String as PS

data PAlgebra a = PAlgebra { getBOps :: [PS.Parser (Binary a)]
                           , getUOps :: [PS.Parser (Unary a)]
                           , getSymbols :: [PS.Parser a]
                           }

type ALexer a =  ReaderT (PAlgebra a) PS.Parser

-- these are just some generic tools for tokenizing algebraic
-- expressions. they lift parsec combiantors to the type "ALexer"

whiteSpace :: PS.Parser String
whiteSpace = P.many $ P.oneOf " \n\r\t"

lexeme :: PS.Parser a -> PS.Parser a
lexeme p = whiteSpace *> p <* whiteSpace


hoist2AR :: (PS.Parser b -> PS.Parser c -> PS.Parser d) ->
           (ALexer a b -> ALexer a c -> ALexer a d)
hoist2AR f = \p q -> (
  ReaderT $ \r -> ((runReaderT p r) `f` (runReaderT q r)))

(<|>) :: ALexer a b -> ALexer a b -> ALexer a b
(<|>) = hoist2AR (P.<|>)

try :: ALexer a b -> ALexer a b
try = hoist P.try

manyTill :: ALexer a b -> ALexer a e -> ALexer a [b]
manyTill = hoist2AR P.manyTill


open :: ALexer a (AToken a)
open = (lift . lexeme . string) "(" >> (return $ Par Open)

close :: ALexer a (AToken a)
close = (lift . lexeme . string) ")" >> (return $ Par Close)

parenLexer = (try open) <|> (try close)


-- these are the alexers, they yield algerbaic tokens
-- lexes symbols
symLexer :: ALexer a (AToken a)
symLexer = ask >>= lift . P.choice . getSymbols >>= return . Sym

--lexes binary ops
binLexer :: ALexer a (AToken a)
binLexer = ask >>= lift . P.choice . getBOps >>= return . Bin

-- lexes unary ops

unLexer :: ALexer a (AToken a)
unLexer = ask >>= lift . P.choice . getUOps >>= return . Un

end :: ALexer a ()
end = do
  lift P.eof

aLexer :: ALexer a (AToken a)
aLexer = try binLexer
      <|> try unLexer
      <|> try symLexer
      <|> try unLexer
      <|> try parenLexer
