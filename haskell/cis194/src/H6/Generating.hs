{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module H6.Generating where

import H6.Stream

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger = flip Cons (streamRepeat 0)
  negate = fmap negate
  (Cons a as) + (Cons b bs) = Cons (a + b) (as + bs)
  as@(Cons a as') * bs@(Cons b bs') = let
    c = a * b
    cs = fmap (a *) bs' + (as' * bs)
    in Cons c cs

instance Fractional (Stream Integer) where
  (Cons a as) / (Cons b bs) = let
    q = Cons (a `div` b) qs
    qs = fmap (`div` b) (as - q * bs)
    in q

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)
