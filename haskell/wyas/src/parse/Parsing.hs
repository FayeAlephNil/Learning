module Parse.Parsing (
  readExpr
)
where

import Lisp

import Parse.NumParse
import Parse.StrParse

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

toStr :: Parser Char -> Parser String
toStr = liftM (:[])

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseChar :: Parser LispVal
parseChar = try $ do
    string "#\\"
    c <- anyChar
    return $ Char c

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _ -> Atom atom

parseList :: Parser LispVal
parseList = try $ liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  first <- endBy parseExpr spaces
  rest  <- char '.' >> spaces >> parseExpr
  return $ DottedList first rest

parseLists :: Parser LispVal
parseLists = do
  char '('
  x <- parseList <|> parseDottedList
  char ')'
  return x

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =  parseNumber
         <|> parseChar
         <|> parseAtom
         <|> parseString
         <|> parseQuoted
         <|> parseLists

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val
