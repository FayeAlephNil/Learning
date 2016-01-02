module Math.Math where

--- Haskell Imports
import Data.List
import Data.Ratio
import Data.Function

--- Package imports
import Data.Digits

--- My imports
import Util.Util

--- Util

-- Returns +1 for a positive number and -1 for a negative number
neg :: (Num a, Ord a)
 	=> a -- number to check
	-> a -- +1 if positive -1 if negative
neg n
	| n < 0 = -1
	| otherwise = 1

-- Checks if a number is an integer
isInt :: RealFrac a
	=> a -- Number to check
	-> Bool -- True if x is an integer, false otherwse
isInt x = x == fromIntegral (round x)

random :: Int
random = 4

-- id = (/1)
divide :: (Integral a) => a -> a -> Double
divide = (/) `on` fromIntegral

--- Divisible

-- Gets all the divisibles of a number
getAllDivisibles :: Integral a
	=> a -- Number to get divisibles of
	-> [a] -- Divisibles
getAllDivisibles n = map (* n) [1..]

-- Takes some divisibles of a number
getSomeDivisibles :: Integral a
	=> Int -- Number to get
	-> a -- Number to get divisibles of
	-> [a] -- Divisibles
getSomeDivisibles y x = take y (getAllDivisibles x)

-- GCD

gcdDomainError = "gcdAll only operates on lists of 2 or more, you passed in a list of length "

-- Gets the GCD of multiple numbers
gcdAll ::
	[Int] -- List of numbers to get GCD of
	-> Int -- GCD
gcdAll [] = error $ gcdDomainError ++ show 0
gcdAll [x] = error $ gcdDomainError ++ show 1
gcdAll [x, y] = gcd x y
gcdAll (x:xs) = gcd x $ gcdAll xs

simplifyAll ::
	[Int] -- List to simplify based on gcd
	-> [Int] -- List of simplified numbers
simplifyAll xs = map (\x -> x `quot` gcdAll xs) xs

--- Factorial

factorial :: (Integral a)
	=> a -- Number to get factorial of
	-> a -- Factorial
factorial n
	| n >= 0 = product [1..n]
	| otherwise = error "You passed a number less than 0 to the factorial function"

--- Fibonacci Sequences

-- Returns a function that gets from the first list for positive indices and the
-- second for negative indices
listAndNega ::
	[a] -- List for positive indices
	-> [a] -- List for negative indices
	-> (Int -> a) -- Function from integer to element
listAndNega list negaList x = if x < 0 then negaList !! (-x) else list !! x
-- Creates a fibonacci sequence with the starting numbers
fibSeq :: (Num a) =>
	a -- First number
	-> a -- Second number
	-> (Int -> a) -- Fibonacci function
fibSeq n m = listAndNega list negaList
	where
		list = n : m : zipWith (+) list (tail list)
		negaList = n : (m - n) : zipWith (-) negaList (tail negaList)

-- Filters a fibonacci sequence
filterFibSeq ::
	((b, Int) -> Bool) -- Function that takes a fib and an index and returns true or false
	-> (Int -> b) -- Fib to filter
	-> (Int -> (b, Int)) -- Result that takes an index and returns the filtered with
											 --a result and regular index. Compose with fst for just result
filterFibSeq f gen = listAndNega list negaList
	where
		list = filter f $ zipWith tuple (map gen [0..]) [0..]
		negaList = filter f $ zipWith tuple (map gen [0,(-1)..]) [0,(-1)..]

-- Regenerates a fib
regenWithGen :: (Num a) =>
	(Int -> a) -- Fibonacci Passed in
	-> ((Int -> a) -> (a, a)) -- Function that takes a fib and returns the starting values
	-> (Int -> a) -- Resulting fib
regenWithGen gen f = uncurry fibSeq start
	where start = f gen

-- Regens given just two indices. Function only has to take nums and generate nums
regenWithIndex :: (Num a) =>
	Int -- First index
	-> Int -- Second index
	-> (Int -> a) -- fib to regen
	-> (a -> a) -- Function to convert from result to new start
	-> Int -> a -- Resulting fib
regenWithIndex n m gen f = regenWithGen gen (\generator -> (f $ generator n, f $ generator m))

-- Regens the fib with indices 0 and 1
regenWith :: (Num a) =>
	(Int -> a) -- fib to regen
	-> (a -> a) -- function that transforms result from fib to new starting value
	-> (Int -> a) -- Resulting fib
regenWith = regenWithIndex 0 1

-- Gets the fibonaccis whose digits summed equals their index
fibDigitSummed :: (Integral a)
	=> (Int -> a) -- Fib to filter
	-> (Int -> (a, Int)) -- Filtered version of fib where the digits summed equals the regular index
fibDigitSummed = filterFibSeq (\(x,y) -> sum (digitsRev 10 (fromIntegral x)) == y)

-- Gens a pisano period
fibPeriodGen :: (Integral a)
	=> (Int -> a) -- Fib to use
	-> a -- Number to use to mod
	-> (Int -> a) -- Resulting function where index will return the modN of the fib
fibPeriodGen f n = modN . gen
	where
		gen = regenWith f modN
		modN x = x `mod` n

-- Fibonacci Numbers
fib = fibSeq 0 1

-- Lists of Fib nums
fibs = map fib [0..]
negaFibs = map fib [0,(-1)..]

-- Pisano period for Fib numbers
fibPeriod = fibPeriodGen fib

-- Lucas numbers
lucas = fibSeq 2 1

-- Lists of Lucas Nums
lucases = map lucas [0..]
negaLucases = map lucas [0,(-1)..]

-- Pisano period for Lucas Nums
lucasPeriod = fibPeriodGen lucas

--- Modulo arithmetic

(#) :: (Integral a) => a -> a -> a
k # n = ((k-1) `mod` n) + 1

--- Digits

-- Gets all the digits of a number added together
-- No ID, information is lost
sumDigits ::
	Int -- Number to get digits from
	-> Int -- digits summed up
sumDigits = sum . digitsRev 10

-- Gets a list of the digits of the number with the smallest absolute value where
-- the digits of said number add up to the number passed in
-- id = sum . minDigitSumList
minDigitSumList ::
	Int -- Number to get the result from
	-> [Int] -- Digits of result
minDigitSumList n = digitSum n : replicate (abs (n `quot` 9)) (neg n * 9)

-- Gets the number with the smallest absolute value where the digits add up to the
-- number passed in
-- id = sumDigits . minDigitSum
minDigitSum ::
	Int -- Number passed in
	-> Int -- Result
minDigitSum = unDigits 10 . minDigitSumList

-- Sums a number's digits until it is a single digit number
-- No ID, information is lost
-- digitSum 9n == 9
-- digitSum (a + b) == digitSum ((digitSum a) + (digitSum b))
-- digitSum (a - b) == digitSum ((digitSum a) - (digitSum b))
-- digitSum (ab) == digitsum ((digitSum a) * (digitSum b))
-- digitSum (polynomial a) == digitSum (polynomial (digitSum a))
digitSum :: (Integral a)
	=> a -- Original number
	-> a -- Single Digit number
digitSum n = neg n * (n # 9)

--- Factors

-- Checks if a number is a factor of another number
factorof :: (Integral a)
	=> a -- Original Number
	-> a -- Possible Factor
	-> Bool -- true if m is a factor of n
factorof m n = (m `mod` n) == 0

-- Gets the factors of the number passed in
factors :: (Integral a) =>
	a -- Number to get factors of
	-> [a] -- Factors of the number
factors n = nub $ sort (below ++ map (\ x -> abs n `quot` x) below)
	where below = [x | x <- [1..(floor $ sqrt $ fromIntegral $ abs n)], n `mod` x == 0]

-- Gets the Prime factors of a number
primeFactors :: (Integral a)
	=> a -- Number to get prime factors of
	-> [a] -- Prime factors of the number
primeFactors = filter prime . factors

factorization :: (Integral a)
	=> a -- Number to get factorization of
	-> [(a, a)] -- Factorization of the number
factorization n
	| prime n = [(n,1)]
	| otherwise = sumSame (head factorizations) (factorizations !! 1)
	where
		factorsN = factors n
		factorizations = map factorization [factorsN !! 1, factorsN !! (length factorsN - 2)]

-- Get the factorPairs of a number
factorPairs :: (Integral a)
	=> a -- Number to get factor pairs of
	-> [(a, a)] -- Factor Pairs
factorPairs = combine . factors

-- Checks if a number is prime
prime :: (Integral a)
	=> a -- Number to check
	-> Bool -- True if the number is prime, else False
prime n = factors n == [1, abs n]

divisor :: (Integral a)
	=> a -- Number to get factors from
	-> a -- Summed Factors
divisor = sum . factors

-- Divisor n / n
charRatio :: (Integral a)
	=> a -- Number to get ratio of
	-> Double -- Ratio
charRatio n = divisor n `divide` n

--- Stern's Diatomic Series

sternNegativeError = "Stern's Diatomic Sequence doesn't have negative indices"

-- Takes an index and gets the resulting number in stern's diatomic series
stern ::
	Int -- Index
	-> Int -- Result
stern n
  | n < 0 = error sternNegativeError
	| otherwise	= sterns !! n

-- List of sterns diatomic Series
sterns = map (stern' 0) [0..]
	where
		-- Tail Recursive Stern function
		stern' :: Int -> Int -> Int
		stern' acc 0 = acc
		stern' acc 1 = stern' (acc + 1) 0
		stern' acc n
			| even n = stern' acc (n `quot` 2)
			| otherwise = let
				m = n-1
				in stern' (acc + stern' 0 ((m `quot` 2) + 1)) (m `quot` 2)

-- List of ratios of the nth number in stern's series over the n + 1 number in the series
sternRatios = zipWith (%) sterns (tail sterns)

-- Gets the ratio of the nth number in the series and the n + 1 number in the series
sternRatio ::
	Int -- Index
	-> Ratio Int -- Ratio
sternRatio n = sternRatios !! n
