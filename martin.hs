module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Data.Char
import Control.Applicative hiding ((<|>), many)
import Data.Functor

data Command = C_Arithmetic Op | C_Push Segment | C_Pop Segment
             | C_Label String | C_Goto String | C_If String | C_Function Function
             | C_Return String | C_Call Function | Comment String  deriving (Show)

type Index = Integer

--general parsing tools

regularParse :: Parser a -> String -> Either ParseError a
regularParse p s = parse p "" s

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t\r"

parseComment :: Parser Command
parseComment = lexeme $ do
  start <- string "//"
  comment <- many1 (noneOf "\n\r")
  return $ Comment (start ++ comment)

parseInt :: Parser Int
parseInt = lexeme $ read <$> (many1 digit)

lexeme :: Parser a -> Parser a
lexeme p =  whitespace *> p <* whitespace

parseLabel :: Parser String
parseLabel = lexeme $ do
  fc <- firstChar
  rest <- restChars
  return $ (fc:rest) where
    firstChar = letter <|> oneOf "_.$:"
    restChars = many1 (letter <|> oneOf "_.:" <|> digit)

choice1 :: [Parser a] -> Parser a
choice1 (x:xs) = foldl ((<|>)) (try x) (fmap try xs)

--command parsers

-- operator

data Op = Unary UOp | Binary BinOp deriving (Show)

data UOp = Neg | Not

instance Show UOp where
  show o = case o of
    Neg -> "-"
    Not -> "!"

parseUOp :: Parser UOp
parseUOp = lexeme $ do
  op <- choice1 $ fmap string ["neg", "not"]
  return $ case op of
    "neg" -> Neg
    "not" -> Not

data ArithOp = Add |Sub | And | Or

instance Show ArithOp where
  show o = case o of
    Add -> "+"
    Sub -> "-"
    And -> "&"
    Or -> "|"

parseArithOp :: Parser ArithOp
parseArithOp = lexeme $ do
  op <- choice1 $ fmap string ["add", "sub", "and", "or"]
  return $ case op of
    "add" -> Add
    "sub" -> Sub
    "and" -> And
    _     -> Or

data BoolOp = Eq | Gt | Lt

instance Show BoolOp where
  show o = case o of
    Eq -> "JEQ"
    Gt -> "JGT"
    Lt -> "JLT"

parseBoolOp :: Parser BoolOp
parseBoolOp = lexeme $ do
  op <- choice1 $ fmap string ["eq", "gt", "lt"]
  return $ case op of
    "eq" -> Eq
    "gt" -> Gt
    "lt" -> Lt

data BinOp = A ArithOp | B BoolOp deriving (Show)

parseBinOp :: Parser BinOp
parseBinOp = lexeme $ (try $ A <$> parseArithOp) <|> (try $ B <$> parseBoolOp)

parseOp :: Parser Op
parseOp = (Unary <$> try parseUOp) <|> (Binary <$> try parseBinOp)

parseC_Arithmetic :: Parser Command
parseC_Arithmetic = C_Arithmetic <$> parseOp

--segement

data Segment = ARG {index :: Int} | LCL {index :: Int} | Static {index :: Int}
             | Constant {index :: Int} | THIS {index ::Int} | THAT {index :: Int}
             | Temp {index :: Int} | Pointer {index :: Int}

instance Show Segment where
  show seg = case seg of
    ARG _ -> "ARG"
    LCL _ -> "LCL"
    Static _ -> "static"
    THIS _ -> "THIS"
    THAT _ -> "THAT"
    Temp _ -> "R5"
    _ -> "pointer"

segments :: [String]
segments = ["argument", "local", "this", "that", "static", "constant", "pointer", "temp"]

parseSegment :: Parser Segment
parseSegment = do
  seg <- lexeme $ choice1 $ fmap string segments
  n <- lexeme $ parseInt
  return $ case seg of
    "argument" -> ARG n
    "local" -> LCL n
    "this" -> THIS n
    "that" -> THAT n
    "static" -> Static n
    "constant" -> Constant n
    "pointer" ->  Pointer n
    "temp" -> Temp n

parseC_Push :: Parser Command
parseC_Push = do
  void $ lexeme $ string "push"
  seg <- parseSegment
  return $ C_Push seg

parseC_Pop :: Parser Command
parseC_Pop = do
  void $ lexeme $ string "pop"
  seg <- parseSegment
  return $ C_Pop seg

-- label

parseC_Label :: Parser Command
parseC_Label = do
  void $ lexeme $ string "label"
  l <- parseLabel
  return $ C_Label l

parseC_Goto :: Parser Command
parseC_Goto = do
  void $ lexeme $ string "goto"
  l <- parseLabel
  return $ C_Goto l

parseC_If :: Parser Command
parseC_If = do
  void $ lexeme $ string "if-goto"
  l <- parseLabel
  return $ C_If l

-- function

data Function = Function { name :: String, nargs :: Int } deriving (Show)

parseFunction :: Parser Function
parseFunction = do
  name <- lexeme $ parseLabel
  argsOrVars <- parseInt
  return $ Function name argsOrVars

parseC_Function :: Parser Command
parseC_Function = do
  void $ lexeme $ string "function"
  f <- parseFunction
  return $ C_Function f

parseC_Call :: Parser Command
parseC_Call = do
  void $ lexeme $ string "call"
  f <- parseFunction
  return $ C_Call f

-- return

parseC_Return :: Parser Command
parseC_Return = C_Return <$> (lexeme $ string "return")

-- combine them all

commandParsers =  [parseC_Arithmetic, parseC_Pop, parseC_Push, parseC_Label
                  , parseC_Goto, parseC_If, parseC_Return, parseC_Function
                  , parseC_Call, parseComment]

parseCommand :: Parser Command
parseCommand = choice1 commandParsers
