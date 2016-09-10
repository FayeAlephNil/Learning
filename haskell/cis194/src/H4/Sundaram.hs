module H4.Sundaram where

import Data.List

sundaram :: Integer -> [Integer]
sundaram n = map ((+1) . (*2)) $ [1..n] \\ lst
  where
    lst = [i + j + 2 * i * j | i <- [1..n], j <- [1..n], i + j + 2 * i * j <= n, i <= j]
