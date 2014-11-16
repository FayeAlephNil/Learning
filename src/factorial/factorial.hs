module Main where

factorial n = if n == 0 then 1 else n * factorial (n - 1)

main = do putStrLn "What number do you want to find the factorial of?"
          x <- readLn
          print (factorial x)
