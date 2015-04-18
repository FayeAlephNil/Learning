module Util.Util where

combine :: [a] -> [(a, a)]
combine [] = []
combine [x] = [(x,x)]
combine (x:xs) = (x, (last xs)):(combine (init xs))
