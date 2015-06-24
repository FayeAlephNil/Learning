module Math.Math where

-- Haskell Imports
import Data.List

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

-- Fibonacci Sequences

fib_seq :: Integer -> Integer -> (Int -> Integer)
fib_seq n m = (\x -> if x < 0 then nega_list !! (-x) else list !! x)
	where
		list = (n : m : zipWith (+) list (tail list)) :: [Integer]
		nega_list = (n : (m-n) : zipWith (-) nega_list (tail nega_list)) :: [Integer]

fib_period_gen :: (Int -> Integer) -> Integer -> (Int -> Integer)
fib_period_gen f n = (\x -> gen x `mod` n)
	where
		gen = fib_seq (f 0 `mod` n) (f 1 `mod` n)

fib = fib_seq 0 1

fibs = map fib [0..]
nega_fibs = map fib [0,(-1)..]

fib_period = fib_period_gen fib

lucas = fib_seq 2 1

lucases = map lucas [0..]
nega_lucases = map lucas [0,(-1)..]

lucas_period = fib_period_gen lucas

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
