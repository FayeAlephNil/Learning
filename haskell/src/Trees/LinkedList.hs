module Trees.LinkedList where

-- Tree Imports
import Trees.Tree

-- Haskell Imports
import Control.Applicative
import Control.Monad

data LinkedList a = EmptyTree | Node a (LinkedList a) deriving (Show, Read, Eq)

instance Tree LinkedList a where
  singleton x = Node x EmptyTree
  empty = EmptyTree
  treeInsert x EmptyTree = singleton x
  treeInsert x rest = Node x rest
  treeToList EmptyTree = []
  treeToList (Node first rest) = first:(treeToList rest)
  treeElem _ _ = False -- Need to move all this stuff around to make Tree more general
  mirror EmptyTree = EmptyTree
  mirror (Node first rest) = treeInsert first (mirror rest)

instance Functor LinkedList where
  fmap f EmptyTree = EmptyTree
  fmap f (Node first rest) = Node (f first) (fmap f rest)

instance Applicative LinkedList where
  pure = singleton
  (<*>) = ap

instance Monad LinkedList where
  EmptyTree >>= g = EmptyTree
  (Node a rest) >>= g = insertAll treeb restTransform
    where
      insertAll EmptyTree base = base
      insertAll (Node x restx) base = treeInsert x (insertAll restx base)
      treeb = g a
      restTransform = rest >>= g
  return = singleton
