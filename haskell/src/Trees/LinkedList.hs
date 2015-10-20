module Trees.LinkedList where

-- Tree Imports
import Trees.Tree

-- Haskell Imports
import Control.Applicative

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
  funcTree <*> EmptyTree = EmptyTree
  EmptyTree <*> valueTree = EmptyTree
  (Node g restg) <*> (Node a resta) = Node b restb
    where
      b = g a
      restb = restg <*> resta

instance Monad LinkedList where
  EmptyTree >>= g = EmptyTree
  (Node a rest) >>= g = Node b restb
    where
      (Node b _rest) = g a
      restb = rest >>= g
  return = singleton
