{-#LANGUAGE FlexibleInstances#-}

module Geometry.ConvexHull2 where

import Polynomial.Monomial
--import Geometry.ConvexHull3


import Data.List
import Debug.Trace
import Data.Matrix hiding (trace)
import qualified Data.Map.Strict as MS
import qualified Data.Sized.Builtin as DS


-- | The algorithm performed in this module is Graham Scan

type Point2D = (Int, Int)


(>*<) :: Int -> (Int, Int) -> (Int, Int)
(>*<) num (px, py) = (num*px, num*py)

instance Num (Int, Int) where
    (x1,y1) + (x2,y2) = (x1+x2, y1+y2)
    negate (x,y) = (-x,-y)
-- above :: Point2D -> Point2D -> Bool
-- above (a,b) (c,d) 
--     | a /= c = False
--     | otherwise = b > d 

-- below :: Point2D -> Point2D -> Bool
-- below (a,b) (c,d) 
--     | a /= c = False
--     | otherwise = b < d 
    

lift2To3 :: Point2D -> (Int, Int, Int)
lift2To3 = \(a,b) -> (a,b,1)


isColinear :: Point2D -> Point2D -> Point2D -> Bool
isColinear a b c =  let 
                        matrix = fromLists (map (\(x,y)-> map toRational [x,y,1]) [a,b,c])
                        res = detLU matrix
                    in res == 0

isColinearFromList :: [Point2D] -> Bool
isColinearFromList [a,b,c] = isColinear a b c



project3To2 :: (Int, Int, Int) -> Point2D
project3To2 = \(a,b,_) -> (a,b)


determinant :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> Rational
determinant p1 p2 p3 = detLU $ fromLists $ map toList [p1,p2,p3]
    where
        toList = (\(a,b,c) -> map toRational [a,b,c])

semiHullUp :: [Point2D] -> Maybe Point2D -> [Point2D]
semiHullUp [a] _= [a]
semiHullUp [a,b] _ = [a,b]
semiHullUp points@(x:y:z:w) Nothing = if determinant x3 y3 z3 < 0 then x:semiHullUp (y:z:w) (Just x)
                        else semiHullUp (x:z:w) Nothing
    where
        (x3:y3:z3:w3) = map lift2To3 points
semiHullUp points@(x:y:z:w) (Just prev) = if determinant x3 y3 z3 < 0 then x:semiHullUp (y:z:w) (Just x)
                        else semiHullUp (prev:x:z:w) Nothing
    where
        (x3:y3:z3:w3) = map lift2To3 points

semiHullDown :: [Point2D] -> Maybe Point2D -> [Point2D]
semiHullDown [a] _ = [a]
semiHullDown [a,b] _ = [a,b]
semiHullDown points@(x:y:z:w) Nothing = if determinant x3 y3 z3 > 0 then x:semiHullDown (y:z:w) (Just x)
                            else semiHullDown (x:z:w) Nothing
    where
        (x3:y3:z3:w3) = map lift2To3 points
semiHullDown points@(x:y:z:w) (Just prev) = if determinant x3 y3 z3 > 0 then x:semiHullDown (y:z:w) (Just x)
                            else semiHullDown (prev:x:z:w) Nothing
    where
        (x3:y3:z3:w3) = map lift2To3 points


-- semiHullDown :: [Point2D] -> [Point2D]
-- semiHullDown [a] = [a]
-- semiHullDown [a,b] = [a,b]
-- semiHullDown points@(x:y:z:w) = if determinant y3 x3 z3 < 0 then x:semiHullDown (y:z:w)
--                             else semiHullDown (x:z:w)
--     where
--         (x3:y3:z3:w3) = map lift2To3 points


leftMost :: [Point2D] -> Point2D
leftMost = foldl1 (\(a,b) (c,d) -> if c < a then (c,d) else (a,b))

rightMost :: [Point2D] -> Point2D
rightMost = foldl1 (\(a,b) (c,d) -> if c > a then (c,d) else (a,b))

isPointUp :: Point2D -> Point2D -> Point2D -> Bool
isPointUp (l1,l2) (r1,r2) (p1,p2) = rP2 >= (m * rP1) + b
    where
        [rL1, rL2, rR1,rR2, rP1, rP2] = map toRational [l1, l2, r1, r2, p1, p2]
        m = (rR2- rL2) / (rR1-rL1)
        b = rL2 - m * rL1

isPointDown :: Point2D -> Point2D -> Point2D -> Bool
isPointDown (l1,l2) (r1,r2) (p1,p2) = rP2 <= (m * rP1) + b
    where
        [rL1, rL2, rR1,rR2, rP1, rP2] = map toRational [l1, l2, r1, r2, p1, p2]
        m = (rR2- rL2) / (rR1-rL1)
        b = rL2 - m * rL1
        

dropColinearPoints :: [Point2D] -> [Point2D]
dropColinearPoints [] = []
dropColinearPoints [a] = [a]
dropColinearPoints [a,b] = [a,b]
dropColinearPoints (x:y:z:w)
    | isColinear x y z = dropColinearPoints (x:z:w)
    | otherwise = x:dropColinearPoints (y:z:w) 


convexHull2 :: [Point2D] -> [Point2D]
convexHull2 points = union lowerHull (reverse upperHull)
        where
            lst = sort $ nub points
            left = leftMost lst
            right = rightMost lst
            lowerHull = dropColinearPoints $ semiHullDown (filter (isPointDown left right) lst) Nothing
            upperHull = dropColinearPoints $ semiHullUp (filter (isPointUp left right) lst) Nothing


    