module H4.Folding where

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node n _ _ _) = n

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node n l node r)
  | height l > height r = Node n l node r'
  | height r > height l = Node n l' node r
  | otherwise           = Node m l' node r
  where
    l' = insert x l
    r' = insert x r
    m = 1 + max (height l') (height r)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

xor :: [Bool] -> Bool
xor = foldr (\x y -> (x || y) && not (x && y)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a bs -> f a : bs) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)
