module Math.Math where

-- Haskell Imports
import Data.Lists

-- My imports
import Util.Util

-- Divisible

getAllDivisibles :: Integral a => a -> [a]
getAllDivisibles n = map (* n) [1..]

getSomeDivisibles :: Integral a => Int -> a -> [a]
getSomeDivisibles y x = take y (getAllDivisibles x)

-- Factorial
factorial :: Integer -> Integer
factorial n
	| n >= 0 = product [1..n]
	| otherwise = error "You passed a number less than 0 to the factorial function"

-- Fibonacci

fib :: Int -> Integer
fib n
	| n < 0     = nega_fibs !! (-n)
	| otherwise = fibs !! n
	where
		fibs = (0 : 1 : zipWith (+) fibs (tail fibs)) :: [Integer]
		nega_fibs = (0 : 1 : zipWith (-) nega_fibs (tail nega_fibs)) :: [Integer]

-- Factors

factorof :: Int -> Int -> Bool
factorof m n = (mod m n) == 0

factors :: Int -> [Int]
factors n = (nub $ sort $ (below ++ (map (\ x -> (abs n) `quot` x) below)))
	where below = [x | x <- [1..(floor $ sqrt $ fromIntegral $ abs $ n)], n `mod` x == 0]

primeFactors :: Int -> [Int]
primeFactors = filter prime . factors

factorPairs :: Int -> [(Int, Int)]
factorPairs = combine . factors

prime :: Int -> Bool
prime n = factors n == [1, (abs n)]
