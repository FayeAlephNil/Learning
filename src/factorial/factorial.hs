module Factorial where

factorial :: Integer -> Integer
factorial n = product [1..n]

main = do putStrLn "What number do you want to find the factorial of?"
          x <- readLn
          print ("\nThe factorial of " ++ show x ++ "is" ++ factorial x)
