module Math.Math where

-- Haskell Imports
import Data.List

-- My imports
import Util.Util

-- Util

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

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

apply_to_fib :: (Int -> Integer) -> (Integer -> a) -> (Int -> a)
apply_to_fib gen f = (\x -> f $ gen x)

regen_with_gen :: (Int -> Integer) -> ((Int -> Integer) -> (Integer, Integer)) -> (Int -> Integer)
regen_with_gen gen f = fib_seq (fst start) (snd start)
	where start = f gen

regen_with_index :: Int -> Int -> (Int -> Integer) -> (Integer -> Integer) -> (Int -> Integer)
regen_with_index n m gen f = regen_with_gen gen (\generator -> ((f $ generator n), (f $ generator m)))

regen_with :: (Int -> Integer) -> (Integer -> Integer) -> (Int -> Integer)
regen_with = regen_with_index 0 1

fib_period_gen :: (Int -> Integer) -> Integer -> (Int -> Integer)
fib_period_gen f n = apply_to_fib gen mod_n
	where
		gen = regen_with f mod_n
		mod_n x = x `mod` n

fib = fib_seq 0 1

fibs = map fib [0..]
nega_fibs = map fib [0,(-1)..]

fib_period = fib_period_gen fib

lucas = fib_seq 2 1

lucases = map lucas [0..]
nega_lucases = map lucas [0,(-1)..]

lucas_period = fib_period_gen lucas

-- Factors

factorof :: Integer -> Integer -> Bool
factorof m n = (mod m n) == 0

factors :: Integer -> [Integer]
factors n = (nub $ sort $ (below ++ (map (\ x -> (abs n) `quot` x) below)))
	where below = [x | x <- [1..(floor $ sqrt $ fromIntegral $ abs $ n)], n `mod` x == 0]

primeFactors :: Integer -> [Integer]
primeFactors = filter prime . factors

factorPairs :: Integer -> [(Integer, Integer)]
factorPairs = combine . factors

prime :: Integer -> Bool
prime n = factors n == [1, (abs n)]

-- Stern's Diatomic Series

stern_negative_error = "Stern's Diatomic Sequence doesn't have negative indices"

stern' :: Int -> Int -> Int
stern' acc 0 = acc
stern' acc 1 = stern' (acc + 1) 0
stern' acc n
	| n < 0 = error stern_negative_error
	| even n = stern' acc (n `quot` 2)
	| otherwise = let
		m = n-1
		in stern' (acc + stern' 0 ((m `quot` 2) + 1)) (m `quot` 2)

stern :: Int -> Int
stern n
	| n < 0 = error stern_negative_error
	| otherwise	= sterns !! n

sterns = map (stern' 0) [0..]

stern_fractionals = zipWith (/) double_sterns (tail double_sterns)
	where
		double_sterns = (map fromIntegral sterns) :: [Double]

stern_fractional :: Int -> Double
stern_fractional n = stern_fractionals !! n
