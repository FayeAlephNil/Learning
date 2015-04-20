module Trees.BinaryTree where

-- Tree Imports
import Trees.Tree

data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Read, Eq)
type OrderedBiTree = (Ord a) => BinaryTree a

instance Functor BinaryTree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

instance (Ord a) => Tree BinaryTree a where
  singleton x = Node x EmptyTree EmptyTree
  empty = EmptyTree
  treeInsert x EmptyTree = singleton x
  treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)
  -- Is not the inverse of treeFromList as the ordering in the List was lost
  treeToList EmptyTree = []
  treeToList (Node x left right) = (treeToList left) ++ (treeToList right) ++ [x]
  treeElem _ EmptyTree = False
  treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right
