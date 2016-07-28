module Main where

import Lisp
import Eval
import Parse.Parsing
import System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
