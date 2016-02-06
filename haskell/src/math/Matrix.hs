module Math.Matrix
( Matrix
, create
, rows
, columns
, scalarMult
) where

import Data.List

data Matrix a = Create [[a]] [[a]] deriving (Show, Read, Eq)

-- (Num a) is a scalar of a matrix

-- Creates a matrix taking each part of an array to be a row
create :: [[a]] -> Matrix a
create rs = Create rs (transpose rs)

rows :: Matrix a -> [[a]]
rows (Create rs _) = rs

columns :: Matrix a -> [[a]]
columns (Create _ cs) = cs

scalarMult :: (Num a) => a -> Matrix a -> Matrix a
scalarMult scalar = fmap (* scalar)

instance Functor Matrix where
  fmap f (Create rs cs) = create rs'
    where
      rs' = map (map f) rs
