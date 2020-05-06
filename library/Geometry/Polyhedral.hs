module Geometry.Polyhedral where

import Util
import Geometry.Vertex (Vertex, extremalVertices, adjacencyMatrix)
import Geometry.Facet
import Polynomial.Monomial
import Polynomial.Prelude
import Polynomial.System

import Data.List
import Data.Matrix
import Data.Maybe
import qualified Data.Bimap as BM
import qualified Data.Map.Strict as MS
import qualified Data.Sized.Builtin as DS



fromAdjacency :: Matrix Bool -> BM.Bimap Int [Int]
fromAdjacency adjacency =  BM.fromList $ zip [1..] $ map indices [1..(nrows adjacency)]
    where
        indices i = [j | j <- [1..(ncols adjacency)], getElem i j adjacency]

-- | Computes the normal cones of a set non necessary extremal.

normalCones :: [Vertex] -> MS.Map Vertex [Hyperplane]
normalCones set = BM.toMap cones
    where
        xtremeSet = extremalVertices set
        dict = BM.fromList $ zip [1..] (sort xtremeSet)
        adjacency = adjacencyMatrix xtremeSet
        vertexNeighs = BM.mapR (map (fromJust . (flip BM.lookup dict))) $ BM.map (fromJust . (flip BM.lookup dict)) $ fromAdjacency adjacency

        transform _ [] = []
        transform v neighs = map ($-$ v) neighs 
        --preCones = BM.mapR (\neighs -> map (\n -> Hyper n 0) neighs) vertexNeighs
        cones = foldr (\v neighs -> BM.adjustWithKey transform v neighs) vertexNeighs (BM.keys vertexNeighs)






lowerFacets :: [(Facet, Hyperplane)] -> [(Facet,Hyperplane)]
lowerFacets [] = []
lowerFacets ((f,h):xs) = if last h < 0 then (f,h) : lowerFacets xs else lowerFacets xs

projection :: [(Facet, Hyperplane)] -> [(Facet,Hyperplane)]
projection [] = []
projection ((f,h):xs) = (f, init h) : projection xs

