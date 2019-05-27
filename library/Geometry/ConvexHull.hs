module Geometry.ConvexHull where

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as MS
import Data.Matrix hiding (trace)
import Data.Ratio
import Debug.Trace


type Point3D = (Ratio Int, Ratio Int, Ratio Int)

-- newtype Vertex = Vertex 
--     {
--         coordinates :: Point3D
--         --incidentEdge :: HalfEdge
--     }

-- data Facet = Facet
--     {
--         idFacet :: Int,
--         outerComponent :: HalfEdge
--     }

-- data HalfEdge = HalfEdge
--     {
--         origin :: Vertex,
--         twin :: HalfEdge,
--        -- incidentFace :: Facet,
--        -- prev :: HalfEdge,
--         next :: HalfEdge
--     }
    
--     | Null


newtype Vertex = Vertex {coordinates :: Point3D} deriving (Eq)

newtype Edge = Edge {vertices :: (Vertex, Vertex)} deriving (Show, Eq)

newtype Facet = Facet {edges :: [Edge]} deriving (Show, Eq)

newtype ConvexHull = ConvexHull {facets :: [Facet]} deriving (Show, Eq)

data ConflictGraph = ConflictGraph {
    verticesF :: (MS.Map Vertex [Facet]),
    facetsV ::  (MS.Map Facet [Vertex])
    }



instance Ord Vertex where
    compare (Vertex v1) (Vertex v2) = compare v1 v2

instance Show Vertex where 
    show (Vertex v) = show v

instance Ord Edge where
    compare (Edge e1) (Edge e2) = compare e1 e2

instance Ord Facet where
    compare (Facet f1) (Facet f2) = compare f1 f2




-- | Assume every point is different
convexHull3D :: [Point3D] -> Maybe [Point3D]
convexHull3D points
    | length points < 4 = Just $ sort points
    | tetraHedron == Nothing = Nothing
    | otherwise = Just $ sort $ fromConvexHull $ addPoints dcel afterTetrahedron conflictGraph
        where
            conflictGraph = startConflictGraph dcel afterTetrahedron
            dcel = (initializeCH . fromJust) tetraHedron
            tetraHedron = computeTetrahedron points
            afterTetrahedron = points \\ (fromJust tetraHedron)

-------------------------------------------------

startConflictGraph :: ConvexHull -> [Point3D] -> ConflictGraph
startConflictGraph convexHull points = foldr (\f conflictGraph -> foldr (\p conflictGraph -> if isInFrontOf f p then insert f (Vertex p) conflictGraph else conflictGraph) (ConflictGraph MS.empty MS.empty) points) (ConflictGraph MS.empty MS.empty) facetsCH
    where
        facetsCH = facets convexHull
--        conflictGraph = MS.empty MS.empty
        insert f p (ConflictGraph vs fs) = ConflictGraph (MS.insertWith (++) p [f] vs) (MS.insertWith (++) f [p] fs) 


initializeCH :: [Point3D] -> ConvexHull
initializeCH [a,b,c,d] = ConvexHull $ map fromVertices [f1,f2,f3,f4]
    where 
        f1 = [a,b,c] -- * Counterclockwise
        f2 = [a,c,d]
        f3 = [a,d,b]
        f4 = [b,d,c]


addPoints :: ConvexHull -> [Point3D] -> ConflictGraph -> ConvexHull
addPoints convexHull [] _ = convexHull
addPoints convexHull (p:ps) conflictGraph
    | inside p convexHull = addPoints convexHull ps conflictGraph
    | otherwise = addPoints (ConvexHull ((facetsCH++newFacets) \\ visibleFaces)) ps newConflictGraph
        where
            facetsCH = facets convexHull

            conflictVertices = verticesF conflictGraph
            conflictFacets = facetsV conflictGraph

            visibleFaces = conflictVertices MS.! (Vertex p)
            horizon = findHorizon visibleFaces
            newFacets = map (fromVertices . (++ [p]) . (\(a,b) -> map coordinates [a,b]) . vertices) horizon

            dropFacets = foldr (\f conflictFacets -> MS.delete f conflictFacets) conflictFacets visibleFaces
            dropVertex = MS.delete (Vertex p) conflictVertices
            -- dropFacetInVertex = map (\f -> MS.update (Just . (delete f)) dropVertex ) visibleFaces
            --dropFacetInVertex = MS.map (\v -> map (\f -> MS.update (Just . (delete f)) v dropVertex) visibleFaces ) dropVertex
            dropFacetInVertex = MS.map (\\visibleFaces) dropVertex

            mapNewFacets = MS.union dropFacets (MS.fromList $ map (\f -> (f,[])) newFacets)

            newConflictGraph = foldr (\f conflictGraph -> foldr (\p conflictGraph -> if isInFrontOf f p then insert f (Vertex p) conflictGraph else conflictGraph) (ConflictGraph MS.empty MS.empty) ps) (ConflictGraph MS.empty MS.empty) newFacets

            insert f p (ConflictGraph vs fs) = ConflictGraph (MS.insertWith (++) p [f] vs) (MS.insertWith (++) f [p] fs) 




findHorizon :: [Facet] -> [Edge]
findHorizon facets =  dropTwins $ concat $ map edges facets
    where 
        dropTwins edges@((Edge (v1,v2)):es) = case elem (Edge (v2,v1)) edges of
                                        True -> dropTwins (edges\\[Edge (v1,v2),Edge (v2,v1)])
                                        False -> (Edge (v1,v2)):(dropTwins es)

-- | Generates a facet from its vertices. Points must be ordered counterclockwise
fromVertices :: [Point3D] -> Facet
fromVertices [a,b,c] = Facet [Edge (va,vb), Edge (vb,vc), Edge (vc,va)]
    where
        [va,vb,vc] = map (Vertex) [a,b,c]
---------------------------------------------------

fromConvexHull :: ConvexHull -> [Point3D]
fromConvexHull convexHull =  map coordinates $ concat $ map (\(Edge (v1,v2)) -> [v1,v2]) $ dropTwins $ concat $ map edges (facets convexHull)     
    where 
        dropTwins [] = []
        dropTwins edges@((Edge (v1,v2)):es) = case elem (Edge (v2,v1)) edges of
                                        True -> dropTwins (edges\\[Edge (v2,v1)])
                                        False -> (Edge (v1,v2)):(dropTwins es)
--
--
--
--
--



---------------------------------------------------------------------------------------------
computeSegment :: [Point3D] -> Maybe [Point3D]
computeSegment [] = Nothing -- * Not enough points or all points are equal
computeSegment (x:xs) = case find (/=x) xs of 
                                Nothing -> Nothing
                                Just y -> Just [x,y] 
      
computeTriangle :: [Point3D] -> Maybe [Point3D]
computeTriangle [] = Nothing
computeTriangle points
    | segment == Nothing = Nothing
    | nicePoint == Nothing = Nothing
    | otherwise = (++) <$> segment <*> (fmap return nicePoint) -- * Applicative functors (<$>, <*>), simple functors (fmap) and monads (return) in action!
    where 
        segment = computeSegment points
        nicePoint = find (not . isColinearIn3D (fromJust segment)) points
   
computeTetrahedron :: [Point3D] -> Maybe [Point3D]
computeTetrahedron [] = Nothing
computeTetrahedron points
    | triangle == Nothing = Nothing
    | nicePoint == Nothing = Nothing
    | otherwise = (++) <$> triangle <*> (fmap return nicePoint) -- * Applicative functors (<$>, <*>), simple functors (fmap) and monads (return) in action!
    where 
        triangle = computeTriangle points
        nicePoint = find (not . isCoplanar (fromJust triangle)) points


isColinearIn3D :: [Point3D] -> Point3D -> Bool -- * Not able to use determinant algorithm because the matrix is not square
isColinearIn3D [a,b] c =    let 
                            distanceXYZ = \(a,b,c) (d,e,f) -> [abs (a-d), abs (b-e), abs (c-f)]
                            ab = distanceXYZ a b
                            ac = distanceXYZ a c
                            ratio = checkRatio ab ac
                            in case ratio of
                                Nothing -> trace "In case Nothing isColinear3D" False
                                Just ls -> trace "In case Just isColinear3D" and $ map (== head ls) (tail ls) 

checkRatio :: [Ratio Int] -> [Ratio Int] -> Maybe [Ratio Int]
checkRatio l1 l2
    | l1 == [0,0,0] = trace "Case 1 checkRatio" Just [0,0,0]
    | l2 == [0,0,0] = trace "Case 2 checkRatio" Just [0,0,0]
    | elem 0 l1' && elem 0 l2' = trace "Case 3 checkRatio" Nothing
    | elem 0 l1' = trace "Case 4 checkRatio" $ Just $ zipWith (/) l1' l2'
    | otherwise = trace "Case 5 checkRatio" $ Just $ zipWith (/) l2' l1'
    where 
        (l1', l2') = filterZeros l1 l2


filterZeros :: [Ratio Int] -> [Ratio Int] -> ([Ratio Int], [Ratio Int])
filterZeros [] [] = ([],[])
filterZeros (x:xs) (y:ys)
        | x == 0 && y == 0 = newXs
        | otherwise = (x:(fst newXs), y:(snd newXs))
        where 
            newXs = filterZeros xs ys


isCoplanar :: [Point3D] -> Point3D -> Bool
isCoplanar [a,b,c] d =    let 
                            matrix = fromLists (map (\(x,y,z)->[x,y,z,1]) [a,b,c,d])
                            res = detLU matrix
                        in res == 0

isInFrontOf :: Facet -> Point3D -> Bool
isInFrontOf facet d =    let
                            [a,b,c] = fromFacet facet
                            matrix = fromLists (map (\(x,y,z)->[x,y,z,1]) [a,b,c,d])
                            res = detLU matrix
                        in res < 0


fromFacet :: Facet -> [Point3D]
fromFacet facet =   let
                        [(a,b),(c,d),(e,f)] = map vertices $ edges facet
                    in map coordinates [a,b,d]

---------------------------------------------------------------------------------------------

inside :: Point3D -> ConvexHull -> Bool
inside p convexHull = and $ map (not.(flip isInFrontOf) p) (facets convexHull)