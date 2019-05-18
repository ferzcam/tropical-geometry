module Geometry.ConvexHull where

import Data.Graph (Graph)
import Data.List
import Data.Maybe
import Data.Matrix hiding (trace)
import Data.Ratio
import Debug.Trace
type Point3D = (Ratio Int, Ratio Int, Ratio Int)

data Vertex = Vertex 
    {
        idVertex :: Int, -- * Identifier must be even
        coordinates :: (Int, Int),
        incidentEdge :: HalfEdge
    }

data Facet = Facet
    {
        idFacet :: Int, -- * Identifier must be odd
        outerComponent :: HalfEdge
    }

data HalfEdge = HalfEdge
    {
        origin :: Vertex,
        twin :: HalfEdge,
        incidentFace :: Facet,
        prev :: HalfEdge,
        next :: HalfEdge
    }

type ConflictGraph = Graph


-- | Assume every point is different
convexHull3D :: [Point3D] -> ConflictGraph -> [Point3D]
convexHull3D points _ =
    | length points < 4 = points
    | otherwise = 
        where
            tetraHedron = fromJust $ computeTetrahedron points
            (x:xs) = points // tetraHedron

    let (properPoints, tetraHedron) = computeTetrahedron points
    in convexHull3D properPoints tetraHedron (makeConflict properPoints tetraHedron)
convexHull3D points cHull conflict

computeTetrahedron :: [Point3D] -> [Point3D]
computeTetrahedron 








computeSegment :: [Point3D] -> Maybe [Point3D]
computeSegment [] = Nothing -- * Not enough points or all points are equal
computeSegment (x:xs) = case find (/=x) xs of 
                                Nothing -> Nothing
                                Just y -> Just [x,y] 
      
computeTriangle :: [Point3D] -> Maybe [Point3D]
computeTriangle [] = Nothing
computeTriangle points
    | segment == Nothing = Nothing
    | nicePoint == Nothing = Nothing
    | otherwise = (++) <$> segment <*> (fmap return nicePoint) -- * Applicative functors (<$>, <*>), simple functors (fmap) and monads (return) in action!
    where 
        segment = computeSegment points
        nicePoint = find (not . isColinearIn3D (fromJust segment)) points
   
computeTetrahedron :: [Point3D] -> Maybe [Point3D]
computeTetrahedron [] = Nothing
computeTetrahedron points
    | triangle == Nothing = Nothing
    | nicePoint == Nothing = Nothing
    | otherwise = (++) <$> triangle <*> (fmap return nicePoint) -- * Applicative functors (<$>, <*>), simple functors (fmap) and monads (return) in action!
    where 
        triangle = computeTriangle points
        nicePoint = find (not . isCoplanar (fromJust triangle)) points


isColinearIn3D :: [Point3D] -> Point3D -> Bool -- * Not able to use determinant algorithm because the matrix is not square
isColinearIn3D [a,b] c =    let 
                            distanceXYZ = \(a,b,c) (d,e,f) -> [abs (a-d), abs (b-e), abs (c-f)]
                            ab = distanceXYZ a b
                            ac = distanceXYZ a c
                            ratio = checkRatio ab ac
                            in case ratio of
                                Nothing -> trace "In case Nothing isColinear3D" False
                                Just ls -> trace "In case Just isColinear3D" and $ map (== head ls) (tail ls) 

checkRatio :: [Ratio Int] -> [Ratio Int] -> Maybe [Ratio Int]
checkRatio l1 l2
    | l1 == [0,0,0] = trace "Case 1 checkRatio" Just [0,0,0]
    | l2 == [0,0,0] = trace "Case 2 checkRatio" Just [0,0,0]
    | elem 0 l1' && elem 0 l2' = trace "Case 3 checkRatio" Nothing
    | elem 0 l1' = trace "Case 4 checkRatio" $ Just $ zipWith (/) l1' l2'
    | otherwise = trace "Case 5 checkRatio" $ Just $ zipWith (/) l2' l1'
    where 
        (l1', l2') = filterZeros l1 l2


filterZeros :: [Ratio Int] -> [Ratio Int] -> ([Ratio Int], [Ratio Int])
filterZeros [] [] = ([],[])
filterZeros (x:xs) (y:ys)
        | x == 0 && y == 0 = newXs
        | otherwise = (x:(fst newXs), y:(snd newXs))
        where 
            newXs = filterZeros xs ys


isCoplanar :: [Point3D] -> Point3D -> Bool
isCoplanar [a,b,c] d =    let 
                            matrix = fromLists (map (\(x,y,z)->[x,y,z,1]) [a,b,c,d])
                            res = detLU matrix
                        in res == 0


