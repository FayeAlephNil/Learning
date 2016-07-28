module Primitives where

import Lisp

unpackNum :: LispVal -> Integer
unpackNum (Integer n) = n
unpackNum _ = 0

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Integer $ foldl1 op $ map unpackNum params

foldOp :: (LispVal -> LispVal -> LispVal) -> [LispVal] -> LispVal
foldOp op params = foldl1 op params

binOp :: (LispVal -> LispVal -> LispVal) -> [LispVal] -> LispVal
binOp f [] = List []
binOp f [x] = List [x]
binOp f (x:y:_) = f x y

singleOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
singleOp f []    = List []
singleOp f (x:_) = f x

lispNot :: LispVal -> LispVal
lispNot (Bool False) = Bool True
lispNot _            = Bool False

lispOr :: LispVal -> LispVal -> LispVal
lispOr (Bool False) (Bool False) = Bool False
lispOr _ _ = Bool True

isInt :: LispVal -> LispVal
isInt (Integer _) = Bool True
isInt _           = Bool False

isReal :: LispVal -> LispVal
isReal (Real _) = Bool True
isReal _        = Bool False

isRational :: LispVal -> LispVal
isRational (Rational {}) = Bool True
isRational _             = Bool False

isComplex :: LispVal -> LispVal
isComplex (Complex {}) = Bool True
isComplex _            = Bool False

isNum :: LispVal -> LispVal
isNum val = isInt val `lispOr` isReal val `lispOr` isRational val `lispOr` isComplex val

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _        = Bool False

isList :: LispVal -> LispVal
isList (List _) = Bool True
isList (DottedList _ rest) = isList rest
isList _        = Bool False

isPair :: LispVal -> LispVal
isPair (DottedList {})  = Bool True
isPair _                = Bool False

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _        = Bool False

isChar :: LispVal -> LispVal
isChar (Char _) = Bool True
isChar _        = Bool False

isStr :: LispVal -> LispVal
isStr (String _) = Bool True
isStr _          = Bool False
