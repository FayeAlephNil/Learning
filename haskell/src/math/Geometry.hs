module Math.Geometry where

type Point = [Int]
type Line = (Point, Point)
type Shape = [Line]


point2D :: a -> a -> Point2D a
point2D a b = Values a (Values b EmptyPoint)

let a = Matrix.create [[3,2], [4,5]]
let b = Matrix.create [[\x -> x + 1, \x -> x * x], [\x -> x * 2, \x -> x * 3]]
let c = [a, Matrix.transpose a, fmap (^ 2) a]
let d = \x -> fmap ($ x) b
