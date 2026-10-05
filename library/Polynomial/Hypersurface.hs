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
import Geometry.Vertex (IVertex)
import Util (safeZipWith)

-- | The tropical hypersurface of a polynomial f is the n-1 skeleton of the Newton polyotpe of f with a regular subdivision induced by a vector w in R^n. The hypersurface will be stored as a set of points.


type Polygon = [Point2D]
type Normals = [Point2D]

-- | This function produces a key-value map of the terms of a polynomial with their corresponding coordinates for the Newton polytope
mapTermPoint :: (IsMonomialOrder ord, Ord k, Integral k) 
    => Polynomial k ord n -> MS.Map Point2D (Monomial ord n, k)
mapTermPoint poly = MS.fromList $ zip points terms 
    where
        terms = MS.toList $ getTerms poly
        monExps = DS.toList . getMonomial
        toPoints (mon,coef) = let [a,b] = monExps mon in (a, b)
        points = map toPoints terms


-- | Solve the two independent equality constraints exactly. The historical
-- plotting API stores Int coordinates, so a rational vertex is rejected rather
-- than silently rounded into a different tropical curve.
computeIntersection :: (Integral k) => (Monomial ord n, k) -> (Monomial ord n, k) -> (Monomial ord n, k) -> Point2D
computeIntersection (mon1, c1) (mon2, c2) (mon3, c3)
    | determinant == 0 = error "computeIntersection: collinear exponents"
    | otherwise = (coordinate xNumerator, coordinate yNumerator)
    where
        [a,b] = map toInteger $ DS.toList $ getMonomial mon1
        [d,e] = map toInteger $ DS.toList $ getMonomial mon2
        [g,h] = map toInteger $ DS.toList $ getMonomial mon3
        u = toInteger c2 - toInteger c1
        v = toInteger c3 - toInteger c1
        determinant = (a-d)*(b-h) - (b-e)*(a-g)
        xNumerator = u*(b-h) - (b-e)*v
        yNumerator = (a-d)*v - u*(a-g)
        coordinate numerator
            | remainder /= 0 = error "computeIntersection: nonintegral tropical vertex is not representable by Point2D"
            | quotient < toInteger (minBound :: Int) || quotient > toInteger (maxBound :: Int) =
                error "computeIntersection: tropical vertex exceeds Point2D range"
            | otherwise = fromInteger quotient
            where (quotient,remainder) = numerator `quotRem` determinant

-- Strict convex boundary in counterclockwise order. Facets are stored as
-- unordered vertex sets; their lexicographic order is not a boundary order.
polygonBoundary :: Polygon -> Polygon
polygonBoundary points
    | length boundary < 3 = error "polygonBoundary: a cell must have affine dimension two"
    | otherwise = boundary
    where
        sorted = sort $ nub points
        half = reverse . foldl' push []
        push (b:a:rest) c | cross a b c <= 0 = push (a:rest) c
        push acc c = c:acc
        boundary = if length sorted < 3 then sorted else init (half sorted) ++ init (half $ reverse sorted)
        cross (ax,ay) (bx,by) (cx,cy) =
            (toInteger bx-toInteger ax)*(toInteger cy-toInteger ay) -
            (toInteger by-toInteger ay)*(toInteger cx-toInteger ax)

polygonEdges :: Polygon -> [(Point2D,Point2D)]
polygonEdges points = zip boundary (tail boundary ++ take 1 boundary)
    where boundary = polygonBoundary points

canonicalEdge :: (Point2D,Point2D) -> (Point2D,Point2D)
canonicalEdge (a,b) = if a <= b then (a,b) else (b,a)

cellVertex :: Integral k => MS.Map Point2D (Monomial ord n,k) -> Polygon -> Point2D
cellVertex pointTerms points
    | all ((== head values) . snd) termValues = vertex
    | otherwise = error "cellVertex: lifted polygon vertices are not coplanar"
    where
        boundary = polygonBoundary points
        (p:q:r:_) = boundary
        term p = fromMaybe (error "cellVertex: missing polynomial term") $ MS.lookup p pointTerms
        vertex@(x,y) = computeIntersection (term p) (term q) (term r)
        value p@(a,b) = toInteger a*toInteger x + toInteger b*toInteger y + toInteger (snd $ term p)
        termValues = [(p,value p) | p <- points]
        values = map snd termValues

findFanNVertex :: Integral k => MS.Map Point2D (Monomial ord n,k) -> Polygon -> (Point2D,Normals)
findFanNVertex pointTerms points = (cellVertex pointTerms points, sort normals)
    where
        boundary = polygonBoundary points
        normals = [innerNormal a b c | (a,b,c) <- zip3 boundary
            (tail boundary ++ take 1 boundary) (drop 2 boundary ++ take 2 boundary)]

findPolygonNVertex :: Integral k => MS.Map Point2D (Monomial ord n,k) -> Polygon -> (Polygon,Point2D)
findPolygonNVertex pointTerms points = (sort points, cellVertex pointTerms points)

-- Use unbounded arithmetic for both orientation and primitive direction:
-- valid Int input coordinates can have products or differences beyond Int.
innerNormal :: Point2D -> Point2D -> Point2D -> Point2D
innerNormal (x1,y1) (x2,y2) (x3,y3)
    | dot == 0 = error "innerNormal: collinear points do not define an inward normal"
    | otherwise = (coordinate $ nx `div` divisor, coordinate $ ny `div` divisor)
    where
        dx = toInteger x2 - toInteger x1
        dy = toInteger y2 - toInteger y1
        dot = dy*(toInteger x3-toInteger x1) - dx*(toInteger y3-toInteger y1)
        (nx,ny) = if dot > 0 then (dy,-dx) else (-dy,dx)
        divisor = gcd nx ny
        coordinate n
            | n < toInteger (minBound :: Int) || n > toInteger (maxBound :: Int) =
                error "innerNormal: primitive normal exceeds Point2D range"
            | otherwise = fromInteger n

innerNormals :: Point2D -> Point2D -> Point2D -> Normals
innerNormals a b c = [innerNormal a b c, innerNormal b c a, innerNormal c a b]

verticesNormals :: (IsMonomialOrder ord, Ord k, Integral k)  => Polynomial k ord n -> MS.Map Point2D Normals
verticesNormals poly = MS.fromList $ map (findFanNVertex polyMap) cells
    where
        polyMap = mapTermPoint poly
        cells = subdivision poly


---- For plotting

-- Historical name retained for callers: cells may now be arbitrary polygons.
neighborTriangles :: [Polygon] -> MS.Map Polygon [Polygon] -> MS.Map Polygon [Polygon]
neighborTriangles polygons initial = foldl' addPair withCells pairs
    where
        cells = nub $ map sort polygons
        withCells = foldl' (\m p -> MS.insertWith (++) p [] m) initial cells
        pairs = [(p,q) | (p:rest) <- tails cells, q <- rest,
            not $ null $ intersect (edges p) (edges q)]
        edges = map canonicalEdge . polygonEdges
        addPair m (p,q) = MS.insertWith union p [q] $ MS.insertWith union q [p] m



pointsWithTriangles :: (IsMonomialOrder ord, Ord k, Integral k)  => Polynomial k ord n -> MS.Map Polygon Point2D
pointsWithTriangles poly = MS.fromList $ map (findPolygonNVertex polyMap) cells
    where
        polyMap = mapTermPoint poly -- MS.Map Point2D (Monomial ord n, k)
        cells = subdivision poly -- [Polygon]
        

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
        
        getEdges ((p,n), []) = map (\normal-> (p, p+ (10 >*< normal))) n
        getEdges ((p,n), (p1,n1):ps) = let ((point, newNormals),edge) = analizeNormals (p,n) (p1,n1)  in
            edge:(getEdges ((point, newNormals), ps))

        pointsWithNormals = attachNormals listMap1

isInverse :: Point2D -> Point2D -> Point2D -> Point2D -> Bool
isInverse (x1,y1) (nx1,ny1) (x2,y2) (nx2,ny2) =
    dx*ny == dy*nx && nx*my == ny*mx &&
    dx*nx + dy*ny > 0 && nx*mx + ny*my < 0
    where
        dx = toInteger x2 - toInteger x1
        dy = toInteger y2 - toInteger y1
        nx = toInteger nx1; ny = toInteger ny1
        mx = toInteger nx2; my = toInteger ny2

    

analizeNormals :: (Point2D, Normals) -> (Point2D, Normals) -> ((Point2D,Normals),(Point2D, Point2D))
analizeNormals (p1, n1) (p2, n2) = ((p1, newNormals), (p1,p2))
    where
        isThereTwin p1 normal p2 normals = any (isInverse p1 normal p2) normals
        newNormals = foldr (\normal acc -> if isThereTwin p1 normal p2 n2 then acc else normal:acc) [] n1

-- Dualize actual cell boundary edges. Triangulating a polygon would invent
-- bounded edges, and matching normal slopes alone loses edge provenance.
hypersurface :: (IsMonomialOrder ord, Ord k, Integral k) => Polynomial k ord n -> [(Point2D,Point2D)]
hypersurface poly = nub $ concatMap dualEdge $ MS.toList incidence
    where
        cells = subdivision poly
        pointTerms = mapTermPoint poly
        center = cellVertex pointTerms
        incidence = MS.fromListWith (++)
            [(canonicalEdge edge,[cell]) | cell <- cells, edge <- polygonEdges cell]
        dualEdge ((a,b),[cell]) =
            let p = center cell
                third = head [c | c <- polygonBoundary cell, c /= a, c /= b]
                normal = innerNormal a b third
            in [(p,p + (10 >*< normal))]
        dualEdge (_,[first,second]) =
            let p = center first; q = center second
            in if p == q then [] else [canonicalEdge (p,q)]
        dualEdge _ = error "hypersurface: nonmanifold subdivision edge"
-- | Exponent vectors of a polynomial, each suffixed with the term's
-- coefficient. Ported from origin/generalTropHyp.
expVecs :: (IsMonomialOrder ord, Real k, Show k, Integral k) => Polynomial k ord n -> [IVertex]
expVecs poly = safeZipWith (++) expVec (map return coeffs)
    where
        terms = (MS.toList . getTerms) poly
        expVec = map ((map toInteger) . DS.toList . getMonomial . fst) terms
        coeffs = map (toInteger . snd) terms
