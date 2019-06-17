module Geometry.ConvexHull where

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as MS
import Data.Matrix hiding (trace)
import Data.Function


type Point3D = (Int, Int, Int)

getX,getY,getZ :: Point3D -> Int

getX (x,_,_) = x
getY (_,y,_) = y
getZ (_,_,z) = z



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

newtype Facet = Facet {edges :: [Edge]}

newtype ConvexHull = ConvexHull {facets :: [Facet]} deriving (Eq)

data ConflictGraph = ConflictGraph {
    verticesF :: MS.Map Vertex [Facet],
    facetsV ::  MS.Map Facet [Vertex]
    } deriving (Show)


instance Show ConvexHull where
    show = show.sort.facets

instance Eq Facet where
    f1 == f2 = ((==) `on` (sort.fromFacet)) f1 f2 

instance Show Facet where
    show = show.fromFacet

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
    | length points < 4 = Just $ sort $ nub points
    | isNothing tetraHedron = Nothing
    | otherwise = Just $ sort $ nub pointsConvexHull
        where
            convexHull =  addPoints initialCH afterTetrahedron conflictGraph
            pointsConvexHull = fromConvexHull convexHull
            conflictGraph = startConflictGraph initialCH afterTetrahedron
            initialCH = (initializeCH . fromJust) tetraHedron
            tetraHedron = computeTetrahedron points
            afterTetrahedron = points \\ fromJust tetraHedron

-------------------------------------------------


startConflictGraph :: ConvexHull -> [Point3D] -> ConflictGraph
startConflictGraph convexHull = matchFacetsPoints facetsCH
    where
       facetsCH = facets convexHull
        
matchFacetsPoints :: [Facet] -> [Point3D] -> ConflictGraph
matchFacetsPoints faces points = foldr (\(f,p) conflictGraph -> insert' f (Vertex p) conflictGraph) (ConflictGraph MS.empty MS.empty) pairsFacetPoint
    where
        pairsFacetPoint = [(f,p) | f <- faces, p <- points, isInFrontOf f p]
        insert' f p (ConflictGraph vs fs)
            | p == Vertex (3,3,3) = ConflictGraph (MS.insertWith (++) p [f] vs) (MS.insertWith (++) f [p] fs) 
            | otherwise = ConflictGraph (MS.insertWith (++) p [f] vs) (MS.insertWith (++) f [p] fs) 

initializeCH :: [Point3D] -> ConvexHull -- ^ Counterclockwise
initializeCH [a,b,c,d] = ConvexHull $ map fromVertices [f1,f2,f3,f4]
    where
        middlePoint = (xs,ys,zs) 
        xs = ((/4).sum) $ map (toRational.getX) [a,b,c,d] 
        ys = ((/4).sum) $ map (toRational.getY) [a,b,c,d] 
        zs = ((/4).sum) $ map (toRational.getZ) [a,b,c,d] 
        isInFront [p1,p2,p3] p4 =   let
                                        completeVectors = map (\(x,y,z)-> map toRational [x,y,z,1]) [p1,p2,p3]
                                        completePoint = (\(x,y,z) -> [x,y,z,1]) p4 
                                        matrix = fromLists (completeVectors ++ [completePoint])
                                        res = detLU matrix
                                    in res < 0

        f1 
            | isInFront [a,b,c] middlePoint = [a,c,b]
            | otherwise = [a,b,c]
        f2 
            | isInFront [a,c,d] middlePoint = [a,d,c]
            | otherwise = [a,c,d]
        f3 
            | isInFront [a,d,b] middlePoint = [a,b,d] 
            | otherwise = [a,d,b]
        f4 
            | isInFront [b,d,c] middlePoint = [b,c,d] 
            | otherwise = [b,d,c]



addPoints :: ConvexHull -> [Point3D] -> ConflictGraph -> ConvexHull
addPoints convexHull [] _ = convexHull
addPoints convexHull (p:ps) conflictGraph
    | inside p convexHull = addPoints convexHull ps conflictGraph
    | otherwise = addPoints (ConvexHull nextFacets) ps newConflictGraph
        where
            facetsCH = facets convexHull

            conflictVertices = verticesF conflictGraph
            
            visibleFaces = conflictVertices MS.! Vertex p
            horizon = findHorizon visibleFaces
            newFacets = map (fromVertices . (++ [p]) . (\(a,b) -> map coordinates [a,b]) . vertices) horizon

            nextFacets = filter (/= Facet []) $ checkCoplanarity (facetsCH\\visibleFaces) newFacets
            newConflictGraph = matchFacetsPoints nextFacets ps
            

checkCoplanarity :: [Facet] -> [Facet] -> [Facet]
checkCoplanarity facets1 facets2 = foldr (\(merged,fs) acc -> (acc \\ fs)++merged) (facets1++facets2) coplanarFaces
            where
                mergeCoplanar f1 f2
                    | length ((pointsFromGinF `on` fromFacet) f1 f2) < 2 = ([],[])
                    | areCoplanarFacets f1 f2 = ([fromVertices $ (mergePoints `on` fromFacet) f1 f2],[f1,f2])
                    | otherwise = ([],[])

                coplanarFaces = map (uncurry mergeCoplanar) [(f1,f2) | f1 <- facets1, f2 <- facets2] 
                pointsFromGinF f= filter (`elem` f)
                


areCoplanarFacets :: Facet -> Facet -> Bool
areCoplanarFacets f1 f2  = isCoplanar (fromFacet f1) (nicePoint f2 f1)
    where
        nicePoint f g = 
            head $ ((\\) `on` fromFacet) f g

findHorizon :: [Facet] -> [Edge]
findHorizon facets =  dropTwins $ concatMap edges facets
    where 
        dropTwins [] = []
        dropTwins edges@(Edge (v1,v2):es) = if Edge (v2,v1) `elem` edges then dropTwins (edges\\[Edge (v1,v2),Edge (v2,v1)])
                                                else Edge (v1,v2):dropTwins es

-- | Generates a facet from its vertices. Points must be ordered counterclockwise
fromVertices :: [Point3D] -> Facet
fromVertices points@(p:ps) = Facet edges
    where
        edges = getEdges $ map Vertex (points++[p])
        getEdges [a] = []
        getEdges (x:y:z) = Edge (x,y):getEdges (y:z)
-- fromVertices [a,b,c] = Facet [Edge (va,vb), Edge (vb,vc), Edge (vc,va)]
--     where
--         [va,vb,vc] = map (Vertex) [a,b,c]
---------------------------------------------------

fromConvexHull :: ConvexHull -> [Point3D]
fromConvexHull convexHull =  concatMap fromFacet (facets convexHull)     
    where 
        dropTwins [] = []
        dropTwins edges@(Edge (v1,v2):es) = if Edge (v2,v1) `elem` es then dropTwins (edges\\[Edge (v2,v1)])
                                                else Edge (v1,v2):dropTwins es



mergePoints :: [Point3D] -> [Point3D] -> [Point3D]
mergePoints p1@(p:ps) p2@(x:y:xs)
    | elem x p1 && elem y p1 = if last p1 == x then checkColinearity $ x:init p1 ++ xs
                                else mergePoints (ps++[p]) p2
    | otherwise = mergePoints p1 ((y:xs) ++ [x])

-- [l1,l2] ++

checkColinearity :: [Point3D] -> [Point3D]
checkColinearity points@(p1:p2:p3:_) = points \\ midPoints
    where
        midPoints = map (\(_,p,_) -> p) nicePoints
        nicePoints = [(p1,p2,p3) | p1 <- points, p2 <- points, p3 <- points, isColinearIn3D [p1,p3] p2, isBetween3D [p1,p3] p2, p1 /= p2, p2/=p3, p1/=p3]



-- checkColinearity :: [Point3D] -> [Point3D]
-- checkColinearity points@(p1:p2:p3:p)
--     | isBetween3D [p1,p3] p2 =  nub $ checkColinearity' ( points ++ [p1,p2])
--     | otherwise = checkColinearity ((p2:p3:p) ++ [p1])
--         where 
--             [l1,l2] = [last (init points), last points]

checkColinearity' :: [Point3D] -> [Point3D]
checkColinearity' [a,b] = []
checkColinearity' points@(p1:p2:p3:p)
    | isColinearIn3D [p1,p3] p2 = checkColinearity' (p1:p3:p)
    | otherwise = p1:checkColinearity' (p2:p3:p)

isBetween3D :: [Point3D] -> Point3D -> Bool
isBetween3D [p1,p3] p2
    | (p2X-p1X)*(p3X-p2X) < 0 = False
    | (p2Y-p1Y)*(p3Y-p2Y) < 0 = False
    | (p2Z-p1Z)*(p3Z-p2Z) < 0 = False
    | otherwise = True
    where
        [p1X, p2X, p3X] = map getX [p1,p2,p3]
        [p1Y, p2Y, p3Y] = map getY [p1,p2,p3]
        [p1Z, p2Z, p3Z] = map getZ [p1,p2,p3]


---------------------------------------------------------------------------------------------
computeSegment :: [Point3D] -> Maybe [Point3D]
computeSegment [] = Nothing -- Not enough points or all points are equal
computeSegment (x:xs) = case find (/=x) xs of 
                                Nothing -> Nothing
                                Just y -> Just [x,y] 
      
computeTriangle :: [Point3D] -> Maybe [Point3D]
computeTriangle [] = Nothing
computeTriangle points
    | isNothing segment = Nothing
    | isNothing nicePoint = Nothing
    | otherwise = (++) <$> segment <*> fmap return nicePoint -- Applicative functors (<$>, <*>), simple functors (fmap) and monads (return) in action!
    where 
        segment = computeSegment points
        nicePoint = find (not . isColinearIn3D (fromJust segment)) points
   
computeTetrahedron :: [Point3D] -> Maybe [Point3D]
computeTetrahedron [] = Nothing
computeTetrahedron points
    | isNothing triangle = Nothing
    | isNothing nicePoint = Nothing
    | otherwise = (++) <$> triangle <*> fmap return nicePoint -- Applicative functors (<$>, <*>), simple functors (fmap) and monads (return) in action!
    where 
        triangle = computeTriangle points
        nicePoint = find (not . isCoplanar (fromJust triangle)) points


isColinearIn3D :: [Point3D] -> Point3D -> Bool -- ^ Not able to use determinant algorithm because the matrix is not square
isColinearIn3D [a,b] c =    let 
                            distanceXYZ (a,b,c) (d,e,f) = [a-d, b-e, c-f]
                            ab = map toRational $ distanceXYZ a b
                            ac = map toRational $ distanceXYZ a c
                            ratio = checkRatio ab ac
                            in case ratio of
                                Nothing -> False
                                Just ls -> all (== head ls) (tail ls) 

checkRatio :: [Rational] -> [Rational] -> Maybe [Rational]
checkRatio l1 l2
    | l1 == [0,0,0] = Just [0,0,0]
    | l2 == [0,0,0] = Just [0,0,0]
    | 0 `elem` l1' && 0 `elem` l2' = Nothing
    | 0 `elem` l1' = Just $ zipWith (/) l1' l2'
    | otherwise = Just $ zipWith (/) l2' l1'
    where 
        (l1', l2') = filterZeros l1 l2


filterZeros :: [Rational] -> [Rational] -> ([Rational], [Rational])
filterZeros [] [] = ([],[])
filterZeros (x:xs) (y:ys)
        | x == 0 && y == 0 = newXs
        | otherwise = (x:fst newXs, y:snd newXs)
        where 
            newXs = filterZeros xs ys


isCoplanar :: [Point3D] -> Point3D -> Bool
isCoplanar (a:b:c:s) d =    let 
                            matrix = fromLists (map (\(x,y,z)-> map toRational [x,y,z,1]) [a,b,c,d])
                            res = detLU matrix
                        in res == 0

isInFrontOf :: Facet -> Point3D -> Bool
isInFrontOf facet d =    let
                            (a:b:c:ps) = fromFacet facet
                            matrix = fromLists (map (\(x,y,z)-> map toRational [x,y,z,1]) [a,b,c,d])
                            res = detLU matrix
                        in res < 0

                        

fromFacet :: Facet -> [Point3D]
fromFacet facet =   let
                        verticesPairs = map vertices $ edges facet
                        verticesFromEdges = map fst verticesPairs
--                        [(a,b),(c,d),(e,f)] = map vertices $ edges facet
                    in map coordinates verticesFromEdges

---------------------------------------------------------------------------------------------

inside :: Point3D -> ConvexHull -> Bool
inside p convexHull = all (not.flip isInFrontOf p) (facets convexHull)

isCoplanarCH :: Point3D -> ConvexHull -> Bool
isCoplanarCH p convexHull = any (`isCoplanar` p) (map fromFacet $ facets convexHull)