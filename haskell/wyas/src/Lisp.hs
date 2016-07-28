module Lisp where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Integer Integer
             | Rational Integer Integer
             | Real Float
             | Complex Float Float
             | String String
             | Bool Bool
             | Char Char
             deriving (Eq)

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

instance Show LispVal where
  show (Atom name) = name
  show (List lst) = "(" ++ unwordsList lst ++ ")"
  show (DottedList first rest) = "(" ++ unwordsList first ++ " . " ++ show rest ++ ")"
  show (Integer int) = show int
  show (Rational n d) = show n ++ " / " ++ show d
  show (Real f) = show f
  show (Complex a b) = show a ++ " + " ++ show b ++ "i"
  show (String contents) = "\"" ++ contents ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
