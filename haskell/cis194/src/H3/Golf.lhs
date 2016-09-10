Code Golf Homework 3
====================

> {-# LANGUAGE OverloadedStrings #-}
> module H3.Golf where
> import Data.List
> import qualified Data.Text as T

Skips
-----

We first create a list of copies (cps) of the given list that is the same length
as the initial list. As an example of the type of the value of cps to be expected
given an input

`[1, 2, 3] => [[1,2,3], [1,2,3], [1,2,3]]`

We then zip that with the human-index of the copies list giving us tuples of
value index pairs. We then apply a filter to each list in which we only accept
values with an index that is divisible by the index of that list. This uses
the same method of zipping the list with the human-index to get tuples.

> skips :: [a] -> [[a]]
> skips xs = [[y | (y, j) <- zip ys [1..], j `mod` i == 0] | (ys, i) <- zip cps [1..]]
>   where
>     cps = replicate (length xs) xs

To begin we define a function which takes an element and a tuple of a Maybe
tuple and a list, this function performs the heavy-lifting. In effect recording
the any local maxima in the second element (the list) and recording the current
locals in the Maybe tuple.

Using this function we may fold over the list, and then take the second element
of the tuple to achieve our final result

> local :: (Ord a) => [a] -> [a]
> local xs = snd $ foldr func ((Nothing, Nothing), []) xs
>   where
>     func z ((Nothing, x), vs) = ((x, Just z), vs)
>     func z ((x, Nothing), vs) = ((Nothing, Just z), vs)
>     func z ((Just x, ys@(Just y)), vs) = ((ys, Just z), if (y > x) && (y > z) then y : vs else vs)

We start by defining the bounds of our list under the name nums. We then define
a function which in effect will compose our \"towers\" of *, this is done
by first doing a right justify over each tower, then transpose the list giving
them as rows of * rather than towers. We then place a newline between each row

After this we simply apply conc to a map on a list which gives the number of times
a number was used and repeats * that many times. This is done through a composition
chain. Finally we append the bottom bar of the histogram, strip any whitespace
and unpack our Text into a String, Text was used do to it providing justifyRight 

> histogram :: [Int] -> String
> histogram xs = T.unpack $ T.strip $
>   let nums = [1..9]
>       conc = T.intercalate "\n" . T.transpose . map (T.justifyRight 20 ' ') in
>     conc (map (flip T.replicate "*" . length . flip filter xs . (==)) nums)
>     `T.append` "\n=========\n123456789"
