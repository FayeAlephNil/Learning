module Util.Util where

combine :: [a] -> [(a, a)]
combine [] = []
combine [x] = [(x,x)]
combine (x:xs) = (x, last xs) : combine (init xs)

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

split :: [a] -> [[a]]
split [] = []
split xs = groupsOf (length xs `quot` 2) xs

tuple :: a -> b -> (a, b)
tuple a b = (a,b)

sumSame :: (Integral a) => [(a,a)] -> [(a,a)] -> [(a,a)]
sumSame [] ys = ys
sumSame xs [] = xs
sumSame (x:xs) yss@(y:ys)
	| fst x == fst y = (fst x, snd x + snd y) : sumSame xs ys
	| otherwise = x : sumSame xs yss
