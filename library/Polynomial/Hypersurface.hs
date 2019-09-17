{-# LANGUAGE DataKinds, TypeFamilies, FlexibleContexts, FlexibleInstances, PolyKinds #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}


module Polynomial.Hypersurface where

import Polynomial.Prelude
import Polynomial.Monomial
import Geometry.ConvexHull2
import qualified Data.Map.Strict as MS
import qualified Data.Sized as DS
import Data.Maybe
import Debug.Trace
import Data.List
import Geometry.ConvexHull3 (Point3D)

-- | The tropical hypersurface of a polynomial f is the n-1 skeleton of the Newton polyotpe of f with a regular subdivision induced by a vector w in R^n. The hypersurface will be stored as a set of points.


-- | This function produces a key-value map of the terms of a polynomial with their corresponding coordinates for the Newton polytope

mapTermPoint :: (IsMonomialOrder ord, Ord k, Integral k) 
    => Polynomial k ord n -> MS.Map Point3D (Monomial ord n, k)
mapTermPoint poly = MS.fromList $ zipWith (,) points terms 
    where
        terms = MS.toList $ (getTerms poly)
        monExps = DS.toList . getMonomial
        toPoints = \(mon,coef) -> let [a,b] = monExps mon in (a, b, fromIntegral coef)
        points = map toPoints terms



-- | Finds the vertex of a tropical line which corresponds to the intersection of the polyhedral fan of a triangle

computeIntersection :: (Integral k) => (Monomial ord n, k) -> (Monomial ord n, k) -> (Monomial ord n, k) -> Point2D
computeIntersection (mon1, c1) (mon2, c2) (mon3, c3) = trace ("A-D: " ++ show (a-d)) (x, y) 
        where 
            [a,b] = trace ("A B is: " ++ show (DS.toList $ getMonomial mon1)) DS.toList $ getMonomial mon1
            [d,e] = trace ("D E is: " ++ show (DS.toList $ getMonomial mon2)) DS.toList $ getMonomial mon2
            [g,h] = trace ("G H is: " ++ show (DS.toList $ getMonomial mon3)) DS.toList $ getMonomial mon3
            c = trace ("C is: " ++ show (fromIntegral c1)) fromIntegral c1
            f = trace ("F is: " ++ show (fromIntegral c2)) fromIntegral c2
            i = trace ("I is: " ++ show (fromIntegral c3)) fromIntegral c3
            y = ((f-c)*(d-g) - (i-f)*(a-d)) `div` ((b-e)*(d-g)-(e-h)*(a-d)) 
            x = ((f-c)-(b-e)*y) `div` (a-d)

findFanVertex :: (Integral k) => MS.Map Point3D (Monomial ord n, k) -> Point3D -> Point3D -> Point3D -> Point2D
findFanVertex mapPointMon p1 p2 p3 = computeIntersection mon1 mon2 mon3 
    where
        [pp1, pp2, pp3] = (reverse.sort) [p1,p2,p3]
        mon1 = fromJust $ MS.lookup pp1 mapPointMon
        mon2 = fromJust $ MS.lookup pp2 mapPointMon
        mon3 = fromJust $ MS.lookup pp3 mapPointMon

        