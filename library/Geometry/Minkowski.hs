module Geometry.Minkowski where

import Geometry.ConvexHull (Point3D)
import Data.List

type Point2D = (Int, Int)

minkowskiSum2D :: [Point2D] ->[Point2D] -> [Point2D]
minkowskiSum2D l1 l2 = sort [sum' a b | a <- l1, b <- l2]

minkowskiSum3D :: [Point3D] ->[Point3D] -> [Point3D]
minkowskiSum3D l1 l2 = sort [sum'' a b | a <- l1, b <- l2]

sum' :: Point2D -> Point2D -> Point2D
sum' (a,b) (c,d) = (a+c,b+d)

sum'' :: Point3D -> Point3D -> Point3D
sum'' (a,b,c) (d,e,f) = (a+d,b+e,c+f)
