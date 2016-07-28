module Parse.StrParse (parseString) where

import Lisp

import Text.ParserCombinators.Parsec hiding (spaces)

escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return $ escape' c
  where
    escape' c = case c of
      'n'  -> "\n"
      'r'  -> "\r"
      'v'  -> "\v"
      't'  -> "\t"
      'b'  -> "\b"
      'f'  -> "\f"
      '0'  -> "\0"
      '\\' -> "\\"
      _    -> ""

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

parseString :: Parser LispVal
parseString = do
    char '"'
    strings <- many character
    char '"'
    return $ String $ concat strings
