module Math.Group where

import Data.Complex

-- Because it's easier
tau :: Double
tau = 2 * pi
i = 0 :+ 1

data Group a = Group {
  members :: [a],
  op :: a -> a -> a,
  e :: a,
  inverse :: a -> a
}

type Homo a b = Group a -> Group b
type Iso a b = (Homo a b, Homo b a)

naturals = [0..]
negs = [-1,-2..]
ints = 0 : concat (zipWith (\x y -> [x, y]) (tail naturals) negs)

-- a & (b & c) = (a & b) & c
-- a & e = a = e & a
-- a & (inverse a) = e = (inverse a) & e
z :: Group Int
z = Group {
  members = ints,
  op = (+),
  e = 0,
  inverse = negate
}

-- Canonical cyclics (mod n)
cyclic :: Int -> Group Int
cyclic n = let
  modn = (`mod` n)
  operation a b = modn (a + b)
  in Group {
    members = [0..(n-1)],
    op = operation,
    e = 0,
    inverse = (n -)
  }

unitRoots :: Int -> Group (Complex Double)
unitRoots n = let
  mkUnit = mkPolar 1.0
  base = mkUnit (tau / fromIntegral n)
  in Group {
    members = map ((base **) . fromIntegral) [0..(n-1)],
    op = (*),
    e = 1,
    inverse = mkUnit . (tau -) . phase
  }
