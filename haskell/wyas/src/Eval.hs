module Eval (eval) where

import Lisp
import Primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("not", singleOp lispNot),
              ("or", foldOp lispOr),
              ("integer?", singleOp isInt),
              ("real?", singleOp isReal),
              ("rational?", singleOp isRational),
              ("complex?", singleOp isComplex),
              ("number?", singleOp isNum),
              ("boolean?", singleOp isBool),
              ("list?", singleOp isList),
              ("pair?", singleOp isPair),
              ("symbol?", singleOp isSymbol),
              ("char?", singleOp isChar),
              ("string?", singleOp isStr)]

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Integer _) = val
eval val@(Bool _) = val
eval val@(Real _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
