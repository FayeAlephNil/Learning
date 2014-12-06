module Divisible where

getAllDivisibles n = [x | x <- [1..], x `mod` n == 0]
getSomeDivisibles y x = take y (getAllDivisibles x)

main = do putStrLn "What number to see if divisible by"
          x <- readLn
          putStrLn "How many do you want?"
          y <- readLn
          print (getSomeDivisibles y x)
