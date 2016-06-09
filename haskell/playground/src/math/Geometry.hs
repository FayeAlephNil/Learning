{-# LANGUAGE NoImplicitPrelude #-}

module Math.Geometry where

import Delude

tau :: Double
tau = 2 * pi

type Point = (Double, Double)
type Line = (Point, Point)

-- Define distance and some point functions

pythag :: (Floating a) => a -> a -> a
pythag a b = sqrt (a ** 2) + (b ** 2)

euDist :: Point -> Point -> Double
euDist (x, y) (x', y') = pythag (x - x') (y - y')

rotate :: Point -> Double -> Point -> Point
rotate (x, y) theta center@(ox, oy) = (c * xdiff - s * ydiff + ox, s * xdiff + c * ydiff + oy)
  where
    xdiff = x - ox
    ydiff = y - oy
    c = cos theta
    s = sin theta

rotateOrigin :: Point -> Double -> Point
rotateOrigin p theta = rotate p theta (0, 0)

-- Figure is just a function telling if a point is on the figure

type Figure = Point -> Bool

-- Some basic figures

circle :: Point -> Double -> Figure
circle c r p = (c `euDist` p) == r

disk :: Point -> Double -> Figure
disk c r p = (c `euDist` p) < r

closedDisk :: Point -> Double -> Figure
closedDisk c r = circle c r `union` disk c r

unitCircle = circle (0, 0) 1
unitDisk = closedDisk (0, 0) 1
flower = unionAll $ map (uncurry $ trans unitDisk) [(0, 1), (0, -1), (1, 0), (-1, 0)]

-- Operations for composing figures, uses Delude to do some things nicely

union :: Figure -> Figure -> Figure
union = (||)

unionAll :: [Figure] -> Figure
unionAll = foldl union (const False)

intersect :: Figure -> Figure -> Figure
intersect = (&&)

diff :: Figure -> Figure -> Figure
diff f g = f && not g

sym :: Figure -> Figure -> Figure
sym f g = union f g `diff` intersect f g

-- Operations to translate figures, just move the point and pass to original function
-- This is a common pattern for moving figures in space

trans :: Figure -> Double -> Double -> Figure
trans f n m (x, y) = f (x - n, y - m)

transHori :: Figure -> Double -> Figure
transHori f n = trans f n 0

transVert :: Figure -> Double -> Figure
transVert f = trans f 0

-- Operations to rotate figures

rotateFigure :: Figure -> Double -> Point -> Figure
rotateFigure f theta c p = f (rotate p (tau - theta) c)

rotateFigureO :: Figure -> Double -> Figure
rotateFigureO f theta = rotateFigure f theta (0, 0)
