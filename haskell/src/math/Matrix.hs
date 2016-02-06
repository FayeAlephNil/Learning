module Math.Matrix
( Matrix
, create
, rows
, columns
, scalarMult
, matrixMult
) where

import Data.List

data Matrix a = Create [[a]] [[a]] deriving (Show, Read, Eq)

-- (Num a) is a scalar of a matrix

-- Creates a matrix taking each part of an array to be a row
create :: [[a]] -> Matrix a
create rs = Create rs (transpose rs)

size :: Matrix a -> (Int, Int)
size (Create rs cs) = (length rs, length cs)

rows :: Matrix a -> [[a]]
rows (Create rs _) = rs

columns :: Matrix a -> [[a]]
columns (Create _ cs) = cs

scalarMult :: (Num a) => a -> Matrix a -> Matrix a
scalarMult scalar = fmap (* scalar)

matrixMult :: (Num a) => Matrix a -> Matrix a -> Maybe (Matrix a)
matrixMult a b
  | snd (size a) == fst (size b) = Just $ create [map (rowColumnProd r) cs | r <- rs]
  | otherwise = Nothing
  where
    rs = rows a
    cs = columns b

rowColumnProd :: (Num a)
  => [a] -- Row
  -> [a] -- Column
  -> a -- Scalar result
rowColumnProd r c = sum $ zipWith (*) r c

instance Functor Matrix where
  fmap f (Create rs cs) = create rs'
    where
      rs' = map (map f) rs
