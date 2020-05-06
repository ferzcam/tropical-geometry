module Geometry.Facet 


where

import Util
import Geometry.Vertex

import Data.Matrix hiding (trace)
import Data.List (sort, delete, nub, isSubsequenceOf)
import qualified Data.Map.Strict as MS
import Data.Maybe
import Debug.Trace
import Linear.Matrix (luSolve)
import  qualified Data.Vector as V


-- | Module that implements functions for facet enumeration. Based on the artcile written by Yaguang Yang: A Facet Enumeration Algorithm for Convex Polytopes. 


type Branch = [Int]
type Facet = [Int]
type Hyperplane = [Rational]


remove :: Eq a => a -> [a] -> [a]
remove elem = (delete elem).nub

centroid :: [Vertex] -> Vertex
centroid set = let n = fromIntegral $ length set 
                in map (/n) (foldr1 (safeZipWith (+)) set)

toOrigin :: [Vertex] -> [Vertex]
toOrigin set = let center = centroid set
                in map ($-$ center) set



computeHyperplane ::
    [Vertex] ->    -- | Set of d d-dimensional vertices.Thus the matrix is square
    (Maybe Hyperplane, Bool)         -- | (Possible hyperplane, Flag indicating in which way the hyperplane was computed)
computeHyperplane [] = (Nothing, False)
computeHyperplane vertices =    if linearSystem /= Nothing 
                                then (linearSystem, False) 
                                else (computeHyperplane' vertices, True)
    where
        linearSystem = fmap V.toList $ solveLS matrix vector
        matrix = fromLists vertices
        vector = V.fromList $ (replicate (length vertices) 1)


computeHyperplane' ::
    [Vertex] ->    -- set of d d-dimensional vertices.Thus the matrix is square
    Maybe Hyperplane         -- hyperplane
computeHyperplane' [] = Nothing
computeHyperplane' vertices
    | length vertices < dim = error "There must be at least d d-dimensional vertices for computing hyperplane"
    | otherwise = Just $ map (detLU.fromLists.(diff ++).return) ident

    where
        dim = length (head vertices)
        points = take dim vertices
        diff = map ($-$ (head points)) (tail points)
        ident = toLists $ identity dim

facetsToVertices :: [Facet] -> MS.Map Int Vertex -> [[Vertex]]
facetsToVertices facets dictIndexVertex = map (take dim) facetsByVertices
    where 
        dim = (length.head.head) facetsByVertices
        facetsByVertices = map (map (fromJust . (flip MS.lookup dictIndexVertex))) facets

generateBranches :: Int -> Int -> AdjacencyMatrix -> [Branch]
generateBranches idx dim adjacency = nub $ map sort $ concatMap (goDeep adjacency dim) (map ((++[idx]).return) neighbors) 
    where
        neighbors = [col | col <- [1..(ncols adjacency)], getElem idx col adjacency]

goDeep :: AdjacencyMatrix -> Int -> Branch -> [Branch]
goDeep adjacency dim b@(x:xs)
    | length b == dim = return b
    | otherwise = concatMap (goDeep adjacency dim) $ (remove []) $ map (smartAppend b) neighbors 
        where
            neighbors = [col | col <- [1..(ncols adjacency)], getElem x col adjacency]
            smartAppend list nElem = if elem nElem list then [] else nElem:list


{- 
    facetEnumetation corresponds to algorithm 2.1 of the aforementioned paper.
    checkVertex corresponds to the outer loop 
    checkBranches corresponds to the inner loop

 -}
facetEnumeration :: 
    [Vertex] ->    -- set of vertices (not centered to origin)
    [(Facet, Hyperplane, Rational)]       -- set of hyperplanes ([[a]], [a])
facetEnumeration vertices = safeZipWith3 (,,) newFacets cleanedHypers b
-- trace ("\n\nINITIAL FACETS: " ++ show facets ++ "\n\nINITIAL HYPER: " ++ show initialHyperplanes)
    where
        uSet = sort $ toOrigin vertices
        center = centroid vertices
        adjacency = adjacencyMatrix uSet
        dictVertexIndex = MS.fromList $ zip uSet [1..]
        dictIndexVertex = MS.fromList $ zip [1..] uSet
        (_,newFacets, newHyperplanes) = foldr (checkVertex dictIndexVertex dictVertexIndex) (adjacency,[], []) uSet
        cleanedHypers = map fromJust $ remove Nothing newHyperplanes
        b = map (succ . (dot center)) cleanedHypers

checkVertex ::
    MS.Map Int Vertex ->    -- ^ dictionany of indices and vertices
    MS.Map Vertex Int ->    -- ^ dictionany of vertices and indices
    Vertex ->               -- ^ vertex to check
    (AdjacencyMatrix,       -- ^ adjacency matrix 
    [Facet],                 -- ^ set of facets
    [Maybe Hyperplane]) ->        -- ^ set of hyperplanes
    (AdjacencyMatrix, [Facet], [Maybe Hyperplane])  -- ^ resulting tuple (adjacency,facets, hyperplanes)
checkVertex dictIndexVertex dictVertexIndex ui (adjacency, facets, hyperplanes) = joinTuple adjacency $ foldr (checkBranch dictIndexVertex ui) (facets, hyperplanes) branches
    where
        idx = fromJust $ MS.lookup ui dictVertexIndex
        dim = length ui
        facetsUi = [facet | facet <- facets, elem idx facet] -- take facets that include vertex ui
        inFacets = \branch -> any (isSubsequenceOf (sort branch)) facetsUi
        branches = filter (not.inFacets) (generateBranches idx dim adjacency)
        joinTuple a (b,c) = (a,b,c)



checkBranch ::
    MS.Map Int Vertex ->    -- ^ dictionary of indices and vertices
    Vertex ->          -- ^ vertex ui that will be the root
    Branch ->   -- ^ branch under ui
    ([Facet],          -- ^ set of facets
    [Maybe Hyperplane]) ->       -- ^ set of hyperplanes
    ([Facet], [Maybe Hyperplane])  -- ^ resulting tuple (facets, hyperplanes)
checkBranch dictIndexVertex ui branch (facets, hyperplanes) =
    if (not.(elem hyper)) hyperplanes && all (<= pure 1) equation12 && (((fmap dot hyper) <*> (pure ui)) > pure 0  || computedWithDet)
    then (branch:facets,hyper:hyperplanes)
    else (facets,hyperplanes)
    where
        branchToVertices = map (\idx -> fromJust $ MS.lookup idx dictIndexVertex) branch
        (hyper, computedWithDet) = computeHyperplane branchToVertices
        set = map snd $ MS.toList dictIndexVertex
        equation12 = map (((fmap dot hyper) <*>) . pure) set
