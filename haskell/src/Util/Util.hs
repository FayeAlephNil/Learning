module Util.Util where

combine :: [a] -> [(a, a)]
combine [] = []
combine [x] = [(x,x)]
combine (x:xs) = (x, last xs) : combine (init xs)

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

splitInto :: Int -> [a] -> [[a]]
splitInto n xs = groupsOf (length xs `quot` n) xs

split :: [a] -> [[a]]
split = splitInto 2

tuple :: a -> b -> (a, b)
tuple a b = (a,b)

sumSame :: (Integral a) => [(a,a)] -> [(a,a)] -> [(a,a)]
sumSame [] ys = ys
sumSame xs [] = xs
sumSame (x:xs) yss@(y:ys)
	| fst x == fst y = (fst x, snd x + snd y) : sumSame xs ys
	| otherwise = x : sumSame xs yss

pairsOf :: [a] -> [b] -> [(a, b)]
pairsOf as bs = [(a, b) | a <- as, b <- bs]

pairsUpTo :: Int -> Int -> [(Int, Int)]
pairsUpTo a b = pairsOf [0..a] [0..b]

pairsDownFrom :: Int -> Int -> [(Int, Int)]
pairsDownFrom a b = pairsOf [a..0] [b..0]

maxLength :: [[a]] -> Int
maxLength xs = maximum $ map length xs

maxIndex :: [[a]] -> Int
maxIndex xs = maxLength xs - 1
