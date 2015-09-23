
module Pear.Lexer.Helper where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr as Ex
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Tok

pearStyle = 
  emptyDef { Tok.commentStart = "#--"
           , Tok.commentEnd = "--#"
           , Tok.commentLine = "#"
           , Tok.nestedComments = True
           , Tok.identStart = letter :: Parser Char
           , Tok.identLetter = alphaNum <|> oneOf "_'"
           , Tok.opStart = operation
           , Tok.opLetter = operation
           , Tok.reservedNames =   [ "if"
                                   , "class", "nil", "true", "false"
                                   , "and", "or"
                                   ]
           , Tok.reservedOpNames = []
           , Tok.caseSensitive = True
           }
           where operation = oneOf ":?!$%*+.<=>@\\/^|-~"


lexer = Tok.makeTokenParser pearStyle

--identifiers
identifier = Tok.identifier lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer

--groupers
parens = Tok.parens lexer
braces = Tok.braces lexer
brackets = Tok.brackets lexer

-- literals
integer = Tok.integer lexer
stringLiteral = Tok.stringLiteral lexer

-- seperators
semi = Tok.semi lexer
comma = Tok.comma lexer
commaSep = Tok.commaSep lexer
whiteSpace = Tok.whiteSpace lexer
lexeme = Tok.lexeme lexer
