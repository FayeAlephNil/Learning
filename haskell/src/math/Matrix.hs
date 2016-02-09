module Math.Matrix
( Matrix
, Matrices
, create
, createColumns
, fromListRows
, fromListColumns
, empty
, size
, rows
, columns
, items
, get
, getAllList
, getAllIndex
, indexPositions
, getAll
, transpose
, add
, scalarMult
, matrixMult
, zipWithMatrix
, zipMatrix
, concatMatrix
) where

import qualified Data.List as List
import qualified Util.Util as Util

import qualified Control.Applicative as App
import qualified Data.Foldable as Fold

data Matrix a = Create [[a]] [[a]] deriving (Eq)
type Matrices a = [Matrix a]

-- (Num a) is a scalar of a matrix

-- Creates a matrix taking each part of an array to be a row
create :: [[a]] -> Matrix a
create rs = Create rs (List.transpose rs)

createColumns :: [[a]] -> Matrix a
createColumns cs = Create (List.transpose cs) cs

fromListRows :: Int -> [a] -> Matrix a
fromListRows i xs = create [xss !! idx | idx <- [0..(i - 1)]]
  where
    xss = Util.splitInto i xs

fromListColumns :: Int -> [a] -> Matrix a
fromListColumns j xs = create [xss !! idx | idx <- [0..(j - 1)]]
  where
    xss = Util.groupsOf j xs

empty :: Matrix a
empty = Create [] []

size :: Matrix a -> (Int, Int)
size (Create rs cs) = (length rs, length cs)

rows :: Matrix a -> [[a]]
rows (Create rs _) = rs

columns :: Matrix a -> [[a]]
columns (Create _ cs) = cs

items :: Matrix a -> [a]
items = concat . rows

get :: Matrix a -> Int -> Int -> a
get (Create rs _) i j = (rs !! i) !! j

getAllList :: [Matrix a] -> Int -> Int -> [a]
getAllList [] _ _ = []
getAllList (m : ms) i j = get m i j : getAllList ms i j

getAllIndex :: Matrix (Matrix a) -> Int -> Int -> Matrix a
getAllIndex (Create [] _) _ _ = empty
getAllIndex m i j = fmap (\x -> get x i j) m

indexPositions :: Int -> Int -> Matrix (Int, Int)
indexPositions _ 0 = empty
indexPositions 0 _ = empty
indexPositions a b = fromListRows a $ Util.pairsUpTo (a - 1) (b - 1)

getAll :: Matrix (Matrix a) -> Matrix (Matrix a)
getAll m = fmap (uncurry $ getAllIndex m) idxPos
  where
    idxPos = uncurry indexPositions $ size m

transpose :: Matrix a -> Matrix a
transpose (Create rs cs) = Create cs rs

add :: (Num a) => Matrix a -> Matrix a -> Maybe (Matrix a)
add a b
  | size a == size b = Just $ zipWithMatrix (+) a b
  | otherwise = Nothing

scalarMult :: (Num a) => a -> Matrix a -> Matrix a
scalarMult scalar = fmap (* scalar)

matrixMult :: (Num a) => Matrix a -> Matrix a -> Maybe (Matrix a)
matrixMult a b
  | snd (size a) == fst (size b) = Just $ create [map (rowColumnProd r) cs | r <- rs]
  | otherwise = Nothing
  where
    rs = rows a
    cs = columns b

rowColumnProd :: (Num a)
  => [a] -- Row
  -> [a] -- Column
  -> a -- Scalar result
rowColumnProd r c = sum $ zipWith (*) r c

zipWithMatrix :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
zipWithMatrix f (Create as _) (Create bs _) = create [zipWith f a b | (a, b) <- zip as bs]

zipMatrix :: Matrix a -> Matrix b -> Matrix (a, b)
zipMatrix = zipWithMatrix Util.tuple

concatMatrix :: Matrix (Matrix a) -> Matrix a
concatMatrix m = fromListRows (fst $ size m) $ concatMap items $ items m
  where
    getSize (Create [] _) = 0
    getSize (Create ((x : xs) : rs) _) = fst $ size x

instance Foldable Matrix where
  foldr f acc (Create rs _) = foldr f acc $ concat rs

instance Functor Matrix where
  fmap f (Create rs cs) = create rs'
    where
      rs' = map (map f) rs

-- TODO, figure out Applicative and Monad instances

-- instance Applicative Matrix where
  -- pure a = Create [[a]] [[a]]
  -- a <*> b = concatMatrix $ fmap (`fmap` b) a

-- instance Monad Matrix where
  -- return = pure
  -- m >>= g = concatMatrix $ fmap g m

instance (Show a) => Show (Matrix a) where
  show (Create [] _) = "[]"
  show (Create rs _) = tail $ foldl (\ acc a -> acc ++ "\n" ++ show a) "" rs
