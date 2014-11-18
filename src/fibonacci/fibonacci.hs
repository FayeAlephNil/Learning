module Main where

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n | n > 0 = fibonacci (n-1) + fibonacci (n-2)
fibonacci n | n < 0 = ((-1)^(-1*n))

main = do putStrLn "What stage of the fibonaccis do you want"
          x <- readLn
          putStrLn ("Stage " ++ show x ++ " of the fibonaccis is " ++ show (fibonacci x))
