module Math.Math where

-- Haskell Imports
import Data.List
import Data.Ratio

-- Package imports
import Data.Digits

-- My imports
import Util.Util

-- Util

neg :: Int -> Int
neg n
	| n < 0 = (-1)
	| otherwise = 1

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

divide :: Int -> Int -> (Int, Int)
a `divide` b = (c, b - a * c)
	where
		c = a `quot` b

random :: Int
random = 4

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
simplifyAll xs = map (\x -> x `quot` (gcdAll xs)) xs

-- Factorial
factorial :: Integer -> Integer
factorial n
	| n >= 0 = product [1..n]
	| otherwise = error "You passed a number less than 0 to the factorial function"

-- Fibonacci Sequences

listAndNega list negaList = (\x -> if x < 0 then negaList !! (-x) else list !! x)

fibSeq :: (Num a) => a -> a -> (Int -> a)
fibSeq n m = listAndNega list negaList
	where
		list = (n : m : zipWith (+) list (tail list))
		negaList = (n : (m-n) : zipWith (-) negaList (tail negaList))

filterFibSeq :: ((b, Int) -> Bool) -> (Int -> b) -> (Int -> (b, Int))
filterFibSeq f gen = listAndNega list negaList
	where
		list = filter f $ zipWith tuple (map gen [0..]) [0..]
		negaList = filter f $ zipWith tuple (map gen [0,(-1)..]) [0,(-1)..]

regenWithGen :: (Num a) => (Int -> a) -> ((Int -> a) -> (a, a)) -> (Int -> a)
regenWithGen gen f = fibSeq (fst start) (snd start)
	where start = f gen

regenWithIndex :: (Num a) => Int -> Int -> (Int -> a) -> (a -> a) -> Int -> a
regenWithIndex n m gen f = regenWithGen gen (\generator -> ((f $ generator n), (f $ generator m)))

regenWith :: (Num a) => (Int -> a) -> (a -> a) -> Int -> a
regenWith = regenWithIndex 0 1

fibDigitSummed :: (Integral a) => (Int -> a) -> (Int -> (a, Int))
fibDigitSummed = filterFibSeq (\(x,y) -> ((sum $ digitsRev 10 (fromIntegral x)) == y))

fibPeriodGen :: (Integral a) => (Int -> a) -> a -> Int -> a
fibPeriodGen f n = modN . gen
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

-- Digits

digitSumList :: Int -> [Int]
digitSumList n = (last ((digits 10 (sum (digits 10 n))))):(replicate (abs (n `quot` 9)) ((neg n) * 9))

digitSum :: Int -> Int
digitSum = (unDigits 10) . digitSumList

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
