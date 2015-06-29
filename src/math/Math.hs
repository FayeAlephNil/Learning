module Math.Math where

-- Haskell Imports
import Data.List
import Data.Ratio

-- My imports
import Util.Util

-- Util

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

divide :: Int -> Int -> (Int, Int)
a `divide` b = (c, b - a * c)
	where
		c = a `quot` b

-- Divisible

getAllDivisibles :: Integral a => a -> [a]
getAllDivisibles n = map (* n) [1..]

getSomeDivisibles :: Integral a => Int -> a -> [a]
getSomeDivisibles y x = take y (getAllDivisibles x)

-- GCD

gcdDomainError = "gcdAll only operates on lists of 2 or more, you passed in a list of length "

gcdAll :: [Int] -> Int
gcdAll [] = error $ gcdDomainError ++ show 0
gcdAll [x] = error $ gcdDomainError ++ show 1
gcdAll [x, y] = gcd x y
gcdAll (x:xs) = gcd x $ gcdAll xs

simplifyAll :: [Int] -> [Int]
simplifyAll = map (\x -> x `quot` (gcdAll xs))

-- Factorial
factorial :: Integer -> Integer
factorial n
	| n >= 0 = product [1..n]
	| otherwise = error "You passed a number less than 0 to the factorial function"

-- Fibonacci Sequences

fibSeq :: Integer -> Integer -> (Int -> Integer)
fibSeq n m = (\x -> if x < 0 then negaList !! (-x) else list !! x)
	where
		list = (n : m : zipWith (+) list (tail list)) :: [Integer]
		negaList = (n : (m-n) : zipWith (-) negaList (tail negaList)) :: [Integer]

applyToFib :: (Int -> Integer) -> (Integer -> a) -> (Int -> a)
applyToFib gen f = (\x -> f $ gen x)

regenWithGen :: (Int -> Integer) -> ((Int -> Integer) -> (Integer, Integer)) -> (Int -> Integer)
regenWithGen gen f = fibSeq (fst start) (snd start)
	where start = f gen

regenWithIndex :: Int -> Int -> (Int -> Integer) -> (Integer -> Integer) -> (Int -> Integer)
regenWithIndex n m gen f = regenWithGen gen (\generator -> ((f $ generator n), (f $ generator m)))

regenWith :: (Int -> Integer) -> (Integer -> Integer) -> (Int -> Integer)
regenWith = regenWithIndex 0 1

fibPeriodGen :: (Int -> Integer) -> Integer -> (Int -> Integer)
fibPeriodGen f n = applyToFib gen modN
	where
		gen = regenWith f modN
		modN x = x `mod` n

fib = fibSeq 0 1

fibs = map fib [0..]
negaFibs = map fib [0,(-1)..]

fibPeriod = fibPeriodGen fib

lucas = fibSeq 2 1

lucases = map lucas [0..]
negaLucases = map lucas [0,(-1)..]

lucasPeriod = fibPeriodGen lucas

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

sternNegativeError = "Stern's Diatomic Sequence doesn't have negative indices"

stern :: Int -> Int
stern n
  | n < 0 = error sternNegativeError
	| otherwise	= sterns !! n

sterns = map (stern' 0) [0..]
	where
		stern' :: Int -> Int -> Int
		stern' acc 0 = acc
		stern' acc 1 = stern' (acc + 1) 0
		stern' acc n
			| even n = stern' acc (n `quot` 2)
			| otherwise = let
				m = n-1
				in stern' (acc + stern' 0 ((m `quot` 2) + 1)) (m `quot` 2)

sternRatios = zipWith (%) sterns (tail sterns)

sternRatio :: Int -> Ratio Int
sternRatio n = sternRatios !! n
