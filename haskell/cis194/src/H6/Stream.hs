module H6.Stream where

import Control.Monad
import Control.Monad.Fix

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

instance Functor Stream where
  fmap f (Cons x s) = Cons (f x) $ fmap f s

streamRepeat :: a -> Stream a
streamRepeat = fix (ap Cons)

streamCycle :: [a] -> Stream a
streamCycle [x] = streamRepeat x
streamCycle (x : xs) = interleaveStreams (streamRepeat x) $ streamCycle xs

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) $ streamMap f s

streamIter :: (a -> a) -> a -> Stream a
streamIter f a = Cons a $ streamIter f $ f a

streamDrop :: Int -> Stream a -> Stream a
streamDrop 0 s = s
streamDrop n (Cons _ s) = streamDrop (n - 1) s

nats :: Stream Integer
nats = streamIter (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x s) (Cons y t) = Cons x (Cons y $ interleaveStreams s t)

streamGet :: Integer -> Stream a -> a
streamGet 0 (Cons x _) = x
streamGet n (Cons _ s) = streamGet (n - 1) s

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) withOnes
  where
    withOnes = interleaveStreams (streamRepeat 1) withTwos
    withTwos = interleaveStreams (streamRepeat 2) $ streamDrop 3 nats
