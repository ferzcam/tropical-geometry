module Geometry.Polyhedral where

import Geometry.ConvexHull3
import Data.List
import Data.Maybe
import Data.Function


-- | Given a vertex of a convex hull, computes the facets adjacent to the vertex.
adjacentFacets :: Point3D -> ConvexHull -> [Facet]
adjacentFacets p c = sortFacets p $ filter (isPointInFacet p) (facets c)


-- | For each vertex of a polyhedron, the facets must be ordered clockwise since we need there normals to point inside the polyhedron

isPointInFacet :: Point3D -> Facet -> Bool
isPointInFacet p f = p `elem` (fromFacet f)


-- | Checks if two facets are adjacent with respect to a vertex in counterclockwise orientation.
areFacetsAdjacent :: Point3D -> Facet -> Facet -> Bool
areFacetsAdjacent p f1 f2 
    | isJust edge1 && isJust edge2 = True
    | otherwise = False
        where
            edge1, edge2 :: Maybe Edge
            edge1 = find (\e -> (coordinates.snd.vertices) e == p) (edges f1)
            edge2 = find (\e -> (coordinates.fst.vertices) e == p) (edges f2)


-- | The facets are ordered counterclockwise with respect to a vertex. [Optimizable]

sortFacets :: Point3D -> [Facet] -> [Facet]
sortFacets _ [a,b] = [a,b]
sortFacets p (f:g:fs)
    | areFacetsAdjacent p f g = f:sortFacets p (g:fs)
    | otherwise = sortFacets p (f:fs)++[g]


-- | Computes normal vector to a facet of the polyhedron with respect to a vertex.
normalVector :: Vertex -> Facet -> Point3D
normalVector v f = crossProduct vector1 vector2
    where
        (v1,v2) = vertices.fromJust $ find (\e -> (fst.vertices) e == v) (edges f) 
        (v3,v4) = vertices.fromJust $ find (\e -> (snd.vertices) e == v) (edges f)
        vector1 = ((-) `on` coordinates) v2 v1
        vector2 = ((-) `on` coordinates) v3 v4


crossProduct :: Point3D -> Point3D -> Point3D
crossProduct (x1,y1,z1) (x2,y2,z2) = (y1*z2-y2*z1,-(x1*z2-x2*z1),x1*y2-x2*y1)


normalCone :: Vertex -> [Facet] -> [Point3D]
normalCone v facets@(f:fs) = computeNormalCone normals
    where
        normals = map (normalVector v) (facets++[f])
        computeNormalCone [v1,v2] = [crossProduct v1 v2]
        computeNormalCone (v1:v2:vs) = (crossProduct v1 v2):computeNormalCone (v2:vs)

normalFan :: ConvexHull -> [[Point3D]]
normalFan convexHull = normalFanPointsFacets (fromConvexHull convexHull) convexHull 

normalFanPointsFacets :: [Point3D] -> ConvexHull -> [[Point3D]]
normalFanPointsFacets [] _ = []
normalFanPointsFacets (v:vs) convexHull = (normalCone (Vertex v) properFacets):normalFanPointsFacets vs convexHull
    where
        properFacets = adjacentFacets v convexHull 