module Main where

factorial n | n < 2 = 1
factorial n = n * factorial (n-1)

main = do putStrLn "What number do you want to find the factorial of?"
          x <- readLn
          print (factorial x)
