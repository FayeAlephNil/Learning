module Trees.Tree where

class (Functor tree) => Tree tree a where
  singleton :: a -> tree a
  empty :: tree a
  treeInsert :: a -> tree a -> tree a
  fromListRooted :: tree a -> [a] -> tree a
  fromListRooted root = foldr treeInsert root
  treeFromList :: [a] -> tree a
  treeFromList = fromListRooted empty
  treeToList :: tree a -> [a]
  treeElem :: a -> tree a -> Bool
  mirror :: tree a -> tree a
