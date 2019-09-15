module Geometry.Polytope where

import Data.List

import Geometry.ConvexHull3
import Geometry.ConvexHull2
import Debug.Trace



-- | Computes the projection R^3 -> R^2 of a convexhull. The input is a convexhull in R^3 and the output will regular subdivision of the convexhull in R^2

projectionToR2 :: ConvexHull -> [[Point2D]]
projectionToR2 convexHull = internal
    where
        facetsInPoints = map fromFacet $ facets convexHull
        triangles = filter (\points -> length points == 3) facetsInPoints
        projected = map (map project3To2) triangles
        border = convexHull2 $ concat projected
        notColinear = filter (\facet -> (not.isColinearFromList) facet) $ projected
        internal = delete' border notColinear 


delete' :: [Point2D] -> [[Point2D]] -> [[Point2D]]
delete' _ [] = []
delete' point (p:ps)
    | sort point == sort p = ps
    | otherwise = p:delete' point ps