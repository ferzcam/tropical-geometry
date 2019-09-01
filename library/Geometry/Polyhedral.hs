module Geometry.Polyhedral where

import Geometry.ConvexHull
import Data.List

-- | For each vertex of a polyhedron, the facets must be ordered clockwise since we need there normals to point inside the polyhedron

isPointInFacet :: Point3D -> Facet -> Bool
isPointInFacet p f = elem p (fromFacet f)


-- | Given a vertex of a convex hull, computes the facets adjacent to the vertex.
adjacentFacets :: Point3D -> ConvexHull -> [Facet]
adjacentFacets p c = sortFacets p $ filter (isPointInFacet p) (facets c)


-- | Checks if two facets are adjacent with respect to a vertex in clockwise orientation.
areFacetsAdjacent :: Point3D -> Facet -> Facet -> Bool
areFacetsAdjacent p f1 f2 
    | edge1 /= Nothing && edge2 /= Nothing = True
    | otherwise = False
        where
            edge1, edge2 :: Maybe Edge
            edge1 = find (\e -> ((coordinates.snd.vertices) e) == p) (edges f1)
            edge2 = find (\e -> ((coordinates.fst.vertices) e) == p) (edges f2)


-- | The facets are ordered clockwise with respect to a vertex. [Optimizable]

sortFacets :: Point3D -> [Facet] -> [Facet]
sortFacets _ [a,b] = [a,b]
sortFacets p (f:g:fs)
    | areFacetsAdjacent p f g = f:(sortFacets p (g:fs))
    | otherwise = sortFacets p (f:fs)++[g]