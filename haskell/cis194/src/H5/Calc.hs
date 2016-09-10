{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module H5.Calc where

import H5.Parser
import qualified Data.Map as M

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

class HasVars a where
  var :: String -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit 0 = False
  lit _ = True
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 x) (Mod7 y) = lit $ x + y
  mul (Mod7 x) (Mod7 y) = lit $ x * y

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add f g x = pure (+) <*> f x <*> g x
  mul f g x = pure (*) <*> f x <*> g x

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

parse :: (Expr e) => String -> Maybe e
parse = parseExp lit add mul

reify :: ExprT -> ExprT
reify = id

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr = fmap (eval . reify) . parse

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
