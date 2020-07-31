{-# LANGUAGE DataKinds, TypeFamilies, FlexibleContexts, FlexibleInstances, PolyKinds #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}

module Polynomial.Hypersurface where

import Geometry.Vertex
import Geometry.Facet (Hyperplane, Facet)
import Polynomial.System
import Polynomial.Prelude
import Polynomial.Monomial
import Polynomial.Variety
import Polynomial.GraphGen
import Geometry.Facet
import Geometry.Polyhedral

import Util
import qualified Data.Map.Strict as MS
import qualified Data.Bimap as BM
import qualified Data.Sized.Builtin as DS
import Data.Matrix hiding (trace)
import Data.Maybe
import Data.List
import Data.Ratio
import Control.Arrow
import Debug.Trace



data Extremal = V Vertex | R IVertex deriving (Eq, Ord)

type EdgeSubdivision = (IVertex, IVertex)

data EdgeHypersurface = Internal (Vertex,Vertex) | External (Vertex,IVertex) deriving (Eq)

instance Show Extremal where
    show (V vertex) = prettyVertex vertex
    show (R ray) = "R " ++ show ray

instance Show EdgeHypersurface where
    show (Internal e) = "I " ++ show e
    show (External e) = "E " ++ show e 


prettyVertex :: Vertex -> String
prettyVertex vertex = "V [" ++ (init rawString) ++ "]"
    where
        num = numerator
        den = denominator
        rawString = foldl (\acc x -> 
                            if den x == 1 then
                                acc ++ (show (num x) ++ ",")
                            else
                                acc ++ (show (num x) ++ "/" ++ show (den x) ++ ",")) 
                    "" vertex


expVecs :: (IsMonomialOrder ord, Real k, Show k, Integral k) => Polynomial k ord n -> [IVertex]
expVecs poly = safeZipWith (++) expVec (map return coeffs)
    where
        terms = (MS.toList . getTerms) poly
        expVec = ((map ((map toInteger) . DS.toList . getMonomial . fst))) terms
        coeffs = map (toInteger.snd) terms


vertices :: (IsMonomialOrder ord, Real k, Show k, Integral k) => Polynomial k ord n -> MS.Map (Vertex) [Int] 
vertices poly = MS.fromList $ safeZipWith (,) (map (fromJust . solveSystem) facetMons) (map fst subdivision)
    where
        terms = (MS.toList . getTerms) poly
        points = expVecs poly
        dictPointTerm = MS.fromList $ zip points terms
        extremal = sort $ extremalVertices points
        dictIndexPoint = MS.fromList $ zip [1..] extremal
        facetEnum = facetEnumeration extremal
        subdivision = lowerFacets $ map (\(a,b,_) -> (a,b)) facetEnum
        facetMons = let 
                        lookUp2 = ((MS.!) dictPointTerm) . ((MS.!) dictIndexPoint)
                    in  map ((map lookUp2).fst) subdivision


lowerHull :: (IsMonomialOrder ord, Real k, Show k, Integral k) => Polynomial k ord n -> [[Vertex]]
lowerHull poly = map (map (map toRational)) pointSub
    where
        terms = (MS.toList . getTerms) poly
        points = expVecs poly
        dictPointTerm = MS.fromList $ zip points terms
        extremal = sort $ extremalVertices points
        dictIndexPoint = MS.fromList $ zip [1..] extremal
        facetEnum = facetEnumeration extremal
        subdivision = lowerFacets $ map (\(a,b,_) -> (a,b)) facetEnum
        pointSub = let 
                        lookUp2 = ((MS.!) dictIndexPoint)
                    in  map ((map lookUp2).fst) subdivision

-- verticesWithRays :: (IsMonomialOrder ord, Real k, Show k) => Polynomial k ord n -> MS.Map Vertex [IVertex]
-- verticesWithRays poly = MS.map (flip selectRays rays) vertices
--     where
--         terms = (MS.toList . getTerms) poly
--         points = expVecs poly
--         dictPointTerm = MS.fromList $ zip points terms
--         extremal = sort $ extremalVertices points
--         dictIndexPoint = MS.fromList $ zip [1..] extremal
--         facetEnum = facetEnumeration extremal
--         subdivision = lowerFacets $ map (\(a,b,_) -> (a,b)) facetEnum
--         facetMons = let 
--                         lookUp2 = ((MS.!) dictPointTerm) . ((MS.!) dictIndexPoint)
--                     in  map ((map lookUp2).fst) subdivision
--         vertices = MS.fromList $ safeZipWith (,) (map (fromJust.solveSystem) facetMons) (map fst subdivision) -- map solveSystem facetMons)
--         rays = (_V_normalCones . _H_normalCones) extremal


verticesWithRaysIndexed :: (IsMonomialOrder ord, Real k, Show k, Integral k) => Polynomial k ord n -> MS.Map Vertex [IVertex]
verticesWithRaysIndexed poly = vertices2
    where
        terms = (MS.toList . getTerms) poly
        points = expVecs poly
        dictPointTerm = MS.fromList $ zip points terms
        extremal = sort $ extremalVertices points
        dictIndexPoint = MS.fromList $ zip [1..] extremal
        facetEnum = facetEnumeration extremal
        subdivision = lowerFacets $ map (\(a,b,_) -> (a,b)) facetEnum
        subdivVertices = map (map (project . fromJust . (flip MS.lookup dictIndexPoint))) (map fst subdivision)
        facetMons = let 
                        lookUp2 = ((MS.!) dictPointTerm) . ((MS.!) dictIndexPoint)
                    in  map ((map lookUp2).fst) subdivision
        vertices = MS.fromList $ safeZipWith (,) (map (fromJust.solveSystem) facetMons) subdivVertices -- map solveSystem facetMons)
        vertices2 = MS.map ((map ((map negate).standard)) . nub . _V_normalCones' . _H_normalCones) vertices
        rays = (_V_normalCones . _H_normalCones) extremal

verticesWithRays :: (IsMonomialOrder ord, Real k, Show k, Integral k) => Polynomial k ord n -> [Extremal]
verticesWithRays poly =  (map V vertices') ++ rays
    where
        terms = (MS.toList . getTerms) poly
        points = expVecs poly
        dictPointTerm = MS.fromList $ zip points terms
        extremal = sort $ extremalVertices points
        dictIndexPoint = MS.fromList $ zip [1..] extremal
        facetEnum = facetEnumeration extremal
        subdivision = lowerFacets $ map (\(a,b,_) -> (a,b)) facetEnum
        subdivVertices = map (map (project . fromJust . (flip MS.lookup dictIndexPoint))) (map fst subdivision)
        facetMons = let 
                        lookUp2 = ((MS.!) dictPointTerm) . ((MS.!) dictIndexPoint)
                    in  map ((map lookUp2).fst) subdivision
        vertices = safeZipWith (,) (map (fromJust.solveSystem) facetMons) subdivVertices -- map solveSystem facetMons)
        dictVerticesEdges = (map.second) ((map ((map negate).standard)) . nub . _V_normalCones' . _H_normalCones) vertices
        rays = onlyRays dictVerticesEdges vertices'
        vertices' = map fst vertices 



verticesWithRaysGraph :: (IsMonomialOrder ord, Real k, Show k, Integral k) => Polynomial k ord n -> MS.Map Vertex [IVertex]
verticesWithRaysGraph poly = vertices2
    where
        terms = (MS.toList . getTerms) poly
        points = expVecs poly
        dictPointTerm = MS.fromList $ zip points terms
        extremal = sort $ extremalVertices points
        dictIndexPoint = MS.fromList $ zip [1..] extremal
        facetEnum = facetEnumeration' extremal
        subdivision = map fst $ lowerFacets' $ map (\(a,b,_) -> (a,b)) facetEnum
        facetMons = let 
                        lookUp2 = ((MS.!) dictPointTerm) 
                    in  map ((map lookUp2)) subdivision
        subdivisionProjected = map (map project) subdivision
        verticesWithCells_V = MS.fromList $ safeZipWith (,) (map (fromJust.solveSystem) facetMons)subdivisionProjected -- map solveSystem facetMons)
        verticesWithCells_H = MS.map facetEnumeration' verticesWithCells_V

        vertices2 = MS.map (map (\(_,b,_) -> ((map negate) . standard) b)) verticesWithCells_H


hypersurface :: (IsMonomialOrder ord, Real k, Show k, Integral k) => Polynomial k ord n -> ([EdgeSubdivision], [EdgeHypersurface])
hypersurface poly = (graphSubdivision subdivisionProjected, graphHypersurface verticesWithCells_H)
    where
        terms = (MS.toList . getTerms) poly
        points = expVecs poly
        dictPointTerm = MS.fromList $ zip points terms
        extremal = sort $ extremalVertices points
        --dictIndexPoint = MS.fromList $ zip [1..] extremal
        subdivision = map fst $ lowerFacets' $ map (\(a,b,_) -> (a,b)) (facetEnumeration' extremal)
        facetMons = let 
                        lookUp2 = ((MS.!) dictPointTerm) 
                    in  map ((map lookUp2)) subdivision
        subdivisionProjected = map (map project) subdivision
        
        verticesWithCells_V = MS.fromList $ safeZipWith (,) (map (fromJust.solveSystem) facetMons)subdivisionProjected -- map solveSystem facetMons)
        verticesWithCells_H = MS.map ((map (\(a,b,_) -> (a,b))) . facetEnumeration') verticesWithCells_V


graphSubdivision :: [[IVertex]] -> [EdgeSubdivision]
graphSubdivision [] = []
graphSubdivision (x:xs) = nub $ [(i,j) | i <- x, j <- x, i < j, isOneFace i j x] ++ graphSubdivision xs

graphHypersurface :: MS.Map Vertex [([IVertex], Hyperplane)] -> [EdgeHypersurface]
graphHypersurface dictionary = MS.foldrWithKey analyzeCell [] dictionary
    where
        analyzeCell vertex cell edges = (findEdges (MS.toList (MS.delete vertex dictionary)) cell) ++ edges
            where
                findEdges [] [] = []
                findEdges [] c
                    | length cell == length c = error "graphHypersurface.analyzeCell.findEdges: cell must produce at least one internal edge" 
                    | otherwise = map (\(_, hyper) -> External (vertex, ((map negate) . standard) hyper)) c
                findEdges ((v2,c2):xs) c
                    | length adjacent == 1 = [Internal (vertex,v2)] ++ findEdges xs (delete (fst $ head adjacent) c)
                    | length adjacent == 0 = findEdges xs c
                    | otherwise = error "graphHypersurface.analyzeCell.findEdges: adjacent cells have only ONE hyperplane in common."
                        where
                            adjacent = [(h1, h2) | h1 <- c, h2 <- c2, (sort.fst) h1 == (sort.fst) h2, snd h1 == ((map negate).snd) h2]

onlyRays :: 
        [(Vertex,[IVertex])]    -- | Dictionary with vertices and their corresponding locally emanating rays
    ->  [Vertex]                -- | List of vertices only
    ->  [Extremal]
onlyRays dict vertices = map R $ foldr (\vertEdges rays -> rays ++ (takeRays vertEdges rays)) [] dict
    where
        takeRays (vertx,edges) rays = filter (\e -> isRay vertx e vertices) ((nub edges)\\rays)


project :: [a] -> [a]
project = init

















selectRays :: [Int] -> MS.Map Int [Vertex] ->  [IVertex]
selectRays vertxs dictVertxRays = nub $ concatMap (takeParallel) pairs  -- filter (\v -> init v /= (replicate ((length v)-1) 0)) $ concat raysPerVertex 
    where
        raysPerVertex =  map (\x -> dictVertxRays MS.! x) vertxs
        pairs = combinations raysPerVertex 2
       -- rays = concatMap (takeParallel) pairs

takeParallel :: [[Vertex]] -> [IVertex]
takeParallel (x:y:z) = map fst equals
    where
        newX = filter (\v -> init v /= (replicate ((length v)-1) 0)) x
        newY = filter (\v -> init v /= (replicate ((length v)-1) 0)) y
        removedLastX = map (\v -> if last v <0 then map negate (init v) else init v) newX
        removedLastY = map (\v -> if last v <0 then map negate (init v) else init v) newY
        pairs = [(standard a, standard b) | a <- removedLastX, b <- removedLastY]
        equals = filter (\(a,b) -> a == b) pairs


standard :: Vertex -> IVertex
standard v = map (numerator . (/commDiv)) noRational 
    where
        commDenom = toRational $ lcmList $ map denominator v
        noRational = (map (*commDenom) v)
        commDiv = toRational $ gcdList (map numerator noRational)
     

---------------------------------------------------------------------------------------------------------------------------------------------
hyperSurfaceGraph :: MS.Map Vertex [IVertex] -> Graph Rational
hyperSurfaceGraph dicVertRays = createGraph nodes
    where 
        listVertRays = MS.toList dicVertRays
        nodes = map (\dic -> Node{pos=fst dic, rays= map (map toRational) (snd dic)}) listVertRays
        
