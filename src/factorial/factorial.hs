module Main where

factorial :: Integer -> Integer
factorial n = product [1..n]  

main = do putStrLn "What number do you want to find the factorial of?"
          x <- readLn
          print (factorial x)
