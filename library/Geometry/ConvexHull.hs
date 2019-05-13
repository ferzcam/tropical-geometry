module ConvexHull where



newtype Vertex = 
    {
        identifier :: Int -- * Identifier must be even
        coordinates :: (Int, Int),
        incidentEdge :: HalfEdge
    }

newtype Facet =
    {
        identifier :: Int -- * Identifier must be odd
        outerComponent :: HalfEdge
    }

newtype HalfEdge =
    {
        origin :: Vertex
        twin :: HalfEdge
        incidentFace :: Facet
        prev :: HalfEdge
        next :: HalfEdge
    }

type ConflictGraph = Graph



convexHull3D :: [[a]] -> [[a]] -> ConflictGraph -> [[a]]
convexHull3D points [] _ = let (properPoints, tetraHedron) = computeTetrahedron points
    in convexHull3D properPoints tetraHedron (makeConflict properPoints tetraHedron)
convexHull3D points cHull conflict
