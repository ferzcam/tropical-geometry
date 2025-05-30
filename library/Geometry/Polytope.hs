{-# LANGUAGE DataKinds, TypeFamilies, FlexibleContexts, FlexibleInstances, PolyKinds #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}

module Geometry.Polytope where

import Data.List

import Geometry.ConvexHull3
import Geometry.ConvexHull2
import Geometry.Polyhedral
import Debug.Trace

import Polynomial.Prelude
import Polynomial.Monomial
import qualified Data.Map.Strict as MS
import qualified Data.Sized as DS

import Data.Maybe



-- | Computes the projection R^3 -> R^2 of a convexhull. The input is a convexhull in R^3 and the output will regular subdivision of the convexhull in R^2

projectionToR2 :: ConvexHull -> [[Point2D]]
projectionToR2 convexHull = let lengthConvexHull = length $ facets convexHull in
                            if lengthConvexHull == 1 then 
                                projected.triangles.facetsInPoints $ facets convexHull
                            else 
                                projected.triangles.facetsInPoints $ lowerFaces
    where
        lowerFaces = filter isLowerFace $ facets convexHull
        facetsInPoints = map fromFacet
        triangles = filter (\points -> length points == 3)
        projected = map (map project3To2) 


delete' :: [Point2D] -> [[Point2D]] -> [[Point2D]]
delete' _ [] = []
delete' point (p:ps)
    | sort point == sort p = ps
    | otherwise = p:delete' point ps


subdivision :: (Integral k) => Polynomial k ord n -> [[Point2D]]
subdivision poly = (projectionToR2.fromJust.convexHull3) points
    where
        terms = MS.toList $ getTerms poly
        monExps = DS.toList . getMonomial
        toPoints (mon, coef) = let [a,b] = monExps mon in (a, b, fromIntegral coef)
        points = map toPoints terms


