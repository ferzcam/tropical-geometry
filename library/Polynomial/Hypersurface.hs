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
import Geometry.Polytope

-- | The tropical hypersurface of a polynomial f is the n-1 skeleton of the Newton polyotpe of f with a regular subdivision induced by a vector w in R^n. The hypersurface will be stored as a set of points.


-- | This function produces a key-value map of the terms of a polynomial with their corresponding coordinates for the Newton polytope


type Polygon = [Point2D]
type Normals = [Point2D]

mapTermPoint :: (IsMonomialOrder ord, Ord k, Integral k) 
    => Polynomial k ord n -> MS.Map Point2D (Monomial ord n, k)
mapTermPoint poly = MS.fromList $ zipWith (,) points terms 
    where
        terms = MS.toList $ (getTerms poly)
        monExps = DS.toList . getMonomial
        toPoints = \(mon,coef) -> let [a,b] = monExps mon in (a, b)
        points = map toPoints terms


-- | Finds the vertex of a tropical line which corresponds to the intersection of the polyhedral fan of a triangle. The output is the result of the systems of equations given by: ax + by +c = 0; dx + ey f = 0; gx + hy + i = 0.

computeIntersection :: (Integral k) => (Monomial ord n, k) -> (Monomial ord n, k) -> (Monomial ord n, k) -> Point2D
computeIntersection (mon1, c1) (mon2, c2) (mon3, c3) = (x, y) 
        where 
            [a,b] = DS.toList $ getMonomial mon1
            [d,e] = DS.toList $ getMonomial mon2
            [g,h] = DS.toList $ getMonomial mon3
            c = fromIntegral c1
            f = fromIntegral c2
            i = fromIntegral c3
            y = ((f-c)*(d-g) - (i-f)*(a-d)) `div` ((b-e)*(d-g)-(e-h)*(a-d)) 
            x = ((f-c)-(b-e)*y) `div` (a-d)

findFanNVertex :: (Integral k) => MS.Map Point2D (Monomial ord n, k) -> Polygon -> (Point2D,Normals)
findFanNVertex mapPointMon points = (computeIntersection mon1 mon2 mon3, sort [inormal1,inormal2,inormal3]) 
    where
        [pp1,pp2,pp3] = sort points -- This sorting is done to ensure that pp1 has non-zero degree on x.
        mon1 = fromJust $ MS.lookup pp3 mapPointMon
        mon2 = fromJust $ MS.lookup pp1 mapPointMon
        mon3 = fromJust $ MS.lookup pp2 mapPointMon
        inormal1 = innerNormal pp1 pp2 pp3        
        inormal2 = innerNormal pp2 pp3 pp1
        inormal3 = innerNormal pp3 pp1 pp2

findPolygonNVertex :: (Integral k) => MS.Map Point2D (Monomial ord n, k) -> Polygon -> (Polygon, Point2D)
findPolygonNVertex mapPointMon points = ( [pp1,pp2,pp3], computeIntersection mon1 mon2 mon3) 
    where
        [pp1,pp2,pp3] = sort points -- This sorting is done to ensure that pp1 has non-zero degree on x.
        mon1 = fromJust $ MS.lookup pp3 mapPointMon
        mon2 = fromJust $ MS.lookup pp1 mapPointMon
        mon3 = fromJust $ MS.lookup pp2 mapPointMon

innerNormal :: Point2D -> Point2D -> Point2D -> Point2D
innerNormal a@(x1,y1) b@(x2,y2) c@(x3,y3)
    | dot > 0 = nab
    | dot < 0 = (y1-y2,x2-x1)
    where
        ab = (x2-x1,y2-y1)
        ac = (x3-x1,y3-y1)
        nab = (y2-y1,x1-x2) -- (y,-x)
        dot = (y2-y1)*(x3-x1) + (x1-x2)*(y3-y1)


innerNormals :: Point2D -> Point2D -> Point2D -> Normals
innerNormals a@(x1,y1) b@(x2,y2) c@(x3,y3) = [inner1, inner2, inner3]
    where
        inner1 = innerNormal a b c
        inner2 = innerNormal b c a
        inner3 = innerNormal c a b
        

verticesNormals :: (IsMonomialOrder ord, Ord k, Integral k)  => Polynomial k ord n -> MS.Map Point2D Normals
verticesNormals poly = MS.fromList $ map (findFanNVertex polyMap) triangles 
    where
        polyMap = mapTermPoint poly
        triangles = subdivision poly


---- For plotting

neighborTriangles :: [Polygon] -> MS.Map Polygon [Polygon] -> MS.Map Polygon [Polygon]
neighborTriangles [] mapContainer = mapContainer
neighborTriangles (p:ps) mapContainer = MS.map (map sort) $ neighborTriangles ps (foldr (lookAndInsert p) mapContainer ps)
    where
        lookAndInsert p1 p2 acc = case length (p1\\p2) == (length p1) - 2 of
                                    True ->  MS.insertWith (++) p2 [p1] $ MS.insertWith (++) p1 [p2] acc
                                    False -> acc



pointsWithTriangles :: (IsMonomialOrder ord, Ord k, Integral k)  => Polynomial k ord n -> MS.Map Polygon Point2D
pointsWithTriangles poly = MS.fromList $ map (findPolygonNVertex polyMap) triangles
    where
        polyMap = mapTermPoint poly -- MS.Map Point2D (Monomial ord n, k)
        triangles = subdivision poly -- [Polygon]
        

polygonCenter :: MS.Map Polygon Point2D -> Polygon -> Point2D
polygonCenter pointPolygonMap polygon = fromJust $ MS.lookup polygon pointPolygonMap

convertMap :: MS.Map Polygon [Polygon] -> MS.Map Polygon Point2D -> MS.Map Point2D [Point2D]
convertMap map1 map2 = MS.fromList $ map fromPolygons $ MS.toList map1
    where 
        fromPolygons (polygon, polygons) = (polygonCenter map2 polygon, map (polygonCenter map2) polygons)


computeEdges :: MS.Map Point2D [Point2D] -> MS.Map Point2D Normals -> [(Point2D, Point2D)]
computeEdges map1 map2 = concatMap getEdges pointsWithNormals
    where
        listMap1 = MS.toList map1
        listMap2 = MS.toList map2
        getNormals point2D = (point2D, fromJust $ MS.lookup point2D map2)
        attachNormals = map (\(e,l) -> (getNormals e, map getNormals l))
        
        getEdges ((p,n), []) = map (\normal-> (p, p+normal)) n
        getEdges ((p,n), (p1,n1):ps) = let ((point, newNormals),edge) = analizeNormals (p,n) (p1,n1)  in
            edge:(getEdges ((point, newNormals), ps))

        pointsWithNormals = attachNormals listMap1

isInverse :: (Point2D) -> (Point2D) -> (Point2D) -> (Point2D) ->Bool
isInverse (x1,y1) (nx1, ny1) (x2,y2) (nx2, ny2)
    | x1 == x2 = nx1 == 0 && nx2 == 0 && ny1*ny2 < 0 
    | y1 == y2 = ny1 == 0 && ny2 == 0 && nx1*nx2 < 0
    | nx1 == 0 = False
    | div (y2-y1) (x2-x1) == div ny1 nx1 = True
    | otherwise = False
    

analizeNormals :: (Point2D, Normals) -> (Point2D, Normals) -> ((Point2D,Normals),(Point2D, Point2D))
analizeNormals (p1, n1) (p2, n2) = ((p1, newNormals), (p1,p2))
    where
        isThereTwin p1 normal p2 normals = any (isInverse p1 normal p2) normals
        newNormals = foldr (\normal acc -> if isThereTwin p1 normal p2 n2 then acc else normal:acc) [] n1

hypersurface :: (IsMonomialOrder ord, Ord k, Integral k)  => Polynomial k ord n -> [(Point2D, Point2D)]
hypersurface poly = nub $ computeEdges (convertMap neighbors pointTriangles) pointNormals
    where 
        pointNormals = verticesNormals poly
        neighbors = neighborTriangles (map sort $ subdivision poly) MS.empty
        pointTriangles = pointsWithTriangles poly