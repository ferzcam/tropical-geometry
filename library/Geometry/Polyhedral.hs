module Geometry.Polyhedral where

import Util
import Geometry.Vertex (Vertex, extremalVertices, adjacencyMatrix)
import Geometry.Facet
import Geometry.LRS

import Polynomial.Monomial
import Polynomial.Prelude
import Polynomial.System

import Data.List
import Data.Matrix hiding (trace)
import Data.Maybe
import qualified Data.Bimap as BM
import qualified Data.Map.Strict as MS
import qualified Data.Sized.Builtin as DS

import Debug.Trace




fromAdjacency :: Matrix Bool -> MS.Map Int [Int]
fromAdjacency adjacency = MS.fromList $ zip [1..] $ map indices [1..(nrows adjacency)]
    where
        indices i = [j | j <- [1..(ncols adjacency)], getElem i j adjacency]

-- | Computes the normal cones of a set non necessary extremal.

_H_normalCones :: [Vertex] -> MS.Map Int [Hyperplane]
_H_normalCones set = MS.mapKeys (fromJust . (flip BM.lookupR dict)) cones  -- BM.toMap cones
    where
        xtremeSet = extremalVertices set
        dict = BM.fromList $ zip [1..] (sort xtremeSet)
        adjacency = adjacencyMatrix xtremeSet
        vertexNeighs = MS.map (map (fromJust . (flip BM.lookup dict))) $ MS.mapKeys (fromJust . (flip BM.lookup dict)) $ fromAdjacency adjacency
--        vertexNeighs = BM.mapR (map (fromJust . (flip BM.lookup dict))) $ fromAdjacency adjacency
        transform _ [] = []
        transform v neighs = map ($-$ v) neighs 
        --preCones = BM.mapR (\neighs -> map (\n -> Hyper n 0) neighs) vertexNeighs
        cones = foldr (\v neighs -> MS.adjustWithKey transform v neighs) vertexNeighs (MS.keys vertexNeighs)


_V_normalCones :: MS.Map Int [Hyperplane] -> MS.Map Int [Vertex]
_V_normalCones set =  MS.map ((\mat -> lrs (-mat) (getB mat) vertx) . fromLists) set
    where
        firstElem = (snd . head . MS.toList) set
        cols = (length.head) firstElem
    --    rows = length firstElem
        getB mat = colFromList (replicate (nrows mat) 0)
        vertx = replicate cols 0

_V_normalCones' :: MS.Map Int [Hyperplane] -> [Vertex]
_V_normalCones' set = concat $ MS.elems $ MS.map ((\mat -> lrs (-mat) b vertx) . fromLists) set
    where
        firstElem = (snd . head . MS.toList) set
        cols = (length.head) firstElem
        rows = length firstElem
        b = colFromList (replicate rows 0)
        vertx = replicate cols 0

lowerFacets :: [(Facet, Hyperplane)] -> [(Facet,Hyperplane)]
lowerFacets [] = []
lowerFacets [a] = [a]
lowerFacets facets = filter (\(f,h) -> last h < 0) facets

projection :: [(Facet, Hyperplane)] -> [(Facet,Hyperplane)]
projection [] = []
projection ((f,h):xs) = (f, init h) : projection xs

