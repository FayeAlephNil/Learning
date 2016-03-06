module Trees.Tree where

class (Functor tree) => Tree tree a where
  singleton :: a -> tree a
  empty :: tree a
  treeInsert :: a -> tree a -> tree a
  fromListRooted :: tree a -> [a] -> tree a
  fromListRooted = foldr treeInsert
  treeFromList :: [a] -> tree a
  treeFromList = fromListRooted empty
  treeToList :: tree a -> [a]
  mirror :: tree a -> tree a

class (Eq a, Tree tree a) => EqTree tree a where
  treeElem :: a -> tree a -> Bool
