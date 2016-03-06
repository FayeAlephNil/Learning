module H1.Hanoi (hanoi) where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi m a c b) ++ [(a, b)] ++ (hanoi m c b a)
  where
    m = n - 1
