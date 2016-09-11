{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module H6.Matrix where

import qualified Data.List as L

newtype Matrix = Rows [[Integer]]

rowTimes :: [Integer] -> [Integer] -> Integer
rowTimes [] [] = 0
rowTimes (x : xs) (y : ys) = (x * y) + rowTimes xs ys

instance Num Matrix where
  fromInteger x = Rows [[x, x], [x, x]]
  negate = Rows . fmap (fmap negate) . rows
  (Rows a) + (Rows b) = Rows $ zipWith (zipWith (+)) a b
  a * b = let
    ar = rows a
    bc = cols b
    r1 = fmap (head ar `rowTimes`) bc
    r2 = fmap (last ar `rowTimes`) bc
    in Rows [r1, r2]

rows :: Matrix -> [[Integer]]
rows (Rows x) = x

cols :: Matrix -> [[Integer]]
cols (Rows x) = L.transpose x

transpose :: Matrix -> Matrix
transpose = Rows . cols

fib4 :: Integer -> Integer
fib4 n = let
  base = Rows [[1, 1], [1, 0]]
  in if n == 0 then 0 else last (head $ rows $ base^n)
