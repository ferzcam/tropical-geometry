{-# LANGUAGE DataKinds, TypeFamilies, FlexibleContexts, FlexibleInstances, PolyKinds #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}

module Polynomial.Hypersurface where

import Geometry.Vertex
import Geometry.Facet (Hyperplane, Facet)
import Polynomial.System
import Polynomial.Prelude
import Polynomial.Monomial
import Polynomial.Variety
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


instance Show Extremal where
    show (V vertex) = prettyVertex vertex
    show (R ray) = "R " ++ show ray


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


expVecs :: (IsMonomialOrder ord, Real k, Show k) => Polynomial k ord n -> [Vertex]
expVecs poly = safeZipWith (++) expVec (map return coeffs)
    where
        terms = (MS.toList . getTerms) poly
        expVec = ((map ((map fromIntegral) . DS.toList . getMonomial . fst))) terms
        coeffs = map (toRational.snd) terms


vertices :: (IsMonomialOrder ord, Real k, Show k) => Polynomial k ord n -> MS.Map (Vertex) [Int] 
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


verticesWithRaysIndexed :: (IsMonomialOrder ord, Real k, Show k) => Polynomial k ord n -> MS.Map Vertex [IVertex]
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

verticesWithRays :: (IsMonomialOrder ord, Real k, Show k) => Polynomial k ord n -> [Extremal]
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


onlyRays :: 
        [(Vertex,[IVertex])]    -- | Dictionary with vertices and their corresponding locally emanating rays
    ->  [Vertex]                -- | List of vertices only
    ->  [Extremal]
onlyRays dict vertices = map R $ foldr (\vertEdges rays -> rays ++ (takeRays vertEdges rays)) [] dict
    where
        takeRays (vertx,edges) rays = filter (\e -> isRay vertx e vertices) ((nub edges)\\rays)


project :: Vertex -> Vertex
project v = init v

















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
     

