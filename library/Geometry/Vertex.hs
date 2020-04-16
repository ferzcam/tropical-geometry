module Geometry.Vertex 

(isExtremeVertex)

where


import Numeric.LinearProgramming
import Data.List ((\\))
import Debug.Trace
import Util

{- 
    Module that implements functions for the finding of all the extreme vertices in the Newton polyhedron of a polynomial f.
-}


-- | Analizes whether a point in a set is extremal or not.
-- | This function is based on lemma 3.1 of the article "LP-Based Solution Algorithms for Convex Hull and Related Problems"
isExtremeVertex :: (Num a, Enum a, Real a) => [a] -> [[a]] -> Bool
isExtremeVertex point set = feasibleNPositive objectiveFunc constraintsLeft constraintsRight
    where
        set2 = set \\ [point]
        differences = map (flip (safeZipWith (-)) point) set2
        tValues = map (negate.pred.(foldr1 (+))) differences
        zValues = repeat (-1)
        objectiveFunc = [-1,1] ++ (replicate (length point) 0)  
        constraintsLeft = ((map.map) realToFrac $ (zipWith (:) zValues $ safeZipWith (:) tValues differences)) ++ [objectiveFunc]
        constraintsRight = (replicate (length set2) 0) ++ [1]



feasibleNPositive :: [Double] -> [[Double]] -> [Double] -> Bool
feasibleNPositive objectiveFunc constraintsLeft constraintsRight = feasNOpt resultLP
-- trace ("SIMPLEX: " ++ show resultLP ) 
    where
        problem = Maximize objectiveFunc
        constraints = Dense $ safeZipWith (:<=:) constraintsLeft constraintsRight
        resultLP = simplex problem constraints []
        feasNOpt (Optimal (optVal, _)) = optVal > 0
        feasNOpt _ = False


