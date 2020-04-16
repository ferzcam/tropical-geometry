module Geometry.Facet 


where

import Util
import Data.Matrix hiding (trace)
import Data.List (sort)
import qualified Data.Map.Strict as MS
import Data.Maybe
import Debug.Trace

-- | Module that implements functions for facet enumeration. Based on the artcile written by Yaguang Yang: A Facet Enumeration Algorithm for Convex Polytopes. 


centroid :: (Fractional a) => [[a]] -> [a]
centroid set = let n = fromIntegral $ length set 
                in map (/n) (foldr1 (safeZipWith (+)) set)

toOrigin :: (Fractional a) => [[a]] -> [[a]]
toOrigin set = let center = centroid set
                in map ($-$ center) set

-- | For reference, review the paper mentioned at the beginning
zeta :: (Fractional a, Eq a) => [a] -> [a] -> [a]
zeta ui uj 
    | uj $-$ ui == (replicate (length ui)) 0 = error "Points must be different to compute zeta"
    | otherwise = let 
                    diff = uj $-$ ui
                    tValue = negate $ (dot ui diff) / (dot diff diff)
                  in
                    ui $+$ (map (*tValue) diff)


hyper :: (Fractional a) => [a] -> [a]
hyper z = map (/(dot z z)) z
                    


areNeighbors :: (Ord a, Fractional a) => [a] -> [a] -> [[a]] -> Bool
areNeighbors ui uj set
    | zValue == replicate (length ui) 0 = False
    | all (<=1) inHalfSpace = True
    | otherwise = False
    where
        zValue = zeta ui uj
        hValue = hyper zValue
        inHalfSpace = map (dot hValue) set
        

generateMatrix :: [[Rational]] -> [[Int]]
generateMatrix set = trace ("\nDICT: " ++ show dict) toLists $ foldr insertNeighbors (zero nPoints nPoints) pairs
    where
        nPoints = length set
        setToOrigin = toOrigin set
        pairs = combinations setToOrigin 2
        dict = MS.fromList $ zip (sort setToOrigin) [1..]
        insertNeighbors [ui, uj] matrix 
            | areNeighbors ui uj setToOrigin = trace ("\nPOINTS: " ++ show i ++ "  " ++ show j) (setElem 1 (i,j)) $ (setElem 1 (j,i)) matrix
            | otherwise = matrix
            where
                i = fromJust $ MS.lookup ui dict
                j = fromJust $ MS.lookup uj dict  

