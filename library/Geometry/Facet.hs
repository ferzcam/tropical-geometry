module Geometry.Facet 


where

import Util
import Data.Matrix hiding (trace)
import Data.List (sort, delete, nub, isSubsequenceOf)
import qualified Data.Map.Strict as MS
import Data.Maybe
import Debug.Trace
import Linear.Matrix (luSolve)
import  qualified Data.Vector as V
--import qualified Numeric.LinearAlgebra as NLA ((<\>), fromLists, fromList, toList)
--import Foreign.Storable (Storable)
--import Data.Ratio
--import Internal.Algorithms (Field)

-- | Module that implements functions for facet enumeration. Based on the artcile written by Yaguang Yang: A Facet Enumeration Algorithm for Convex Polytopes. 


type Vertex = [Rational]
type Branch = [Int]
type AdjacencyMatrix = Matrix Bool
type Facet = [Int]
type Hyperplane = [Rational]


centroid :: [Vertex] -> Vertex
centroid set = let n = fromIntegral $ length set 
                in map (/n) (foldr1 (safeZipWith (+)) set)

toOrigin :: [Vertex] -> [Vertex]
toOrigin set = let center = centroid set
                in map ($-$ center) set

-- | For reference, review the paper mentioned at the beginning
zeta :: Vertex -> Vertex -> Vertex
zeta ui uj 
    | uj $-$ ui == (replicate (length ui)) 0 = error "Points must be different to compute zeta"
    | otherwise = let 
                    diff = uj $-$ ui
                    tValue = negate $ (dot ui diff) / (dot diff diff)
                  in
                    ui $+$ (map (*tValue) diff)


hyper :: Vertex -> Hyperplane
hyper z = map (/(dot z z)) z
                    


areNeighbors :: Vertex -> Vertex -> MS.Map Vertex Int -> Maybe Facet
areNeighbors ui uj dict
    | zValue == replicate (length ui) 0 = Nothing
    | all (<=1) inHalfSpace = trace ("INDICES: " ++ show indices) Just (sort indices)
    | otherwise = Nothing
    where
        set = MS.keys dict
        dimension = length ui
        zValue = zeta ui uj
        hValue = hyper zValue
        inHalfSpace = map (dot hValue) set
        inFacet = filter (\x -> dot hValue x == 1) set
        indices =   if (length inFacet) >= dimension 
                    then map (fromJust . (flip MS.lookup dict)) inFacet 
                    else []
        

-- | Generates the adjacency matrix of vertices and the initial facet matrix. The centroid is computed inside the function.
generateMatrices :: [Vertex] -> (AdjacencyMatrix, [Facet])
generateMatrices set =  foldr insertNeighbors ((zero nPoints nPoints),[]) pairs
-- trace ("\nDICT: " ++ show dict)
    where
        nPoints = length set
        dimension = length (head set)
        setToOrigin = toOrigin set
        pairs = combinations setToOrigin 2
        dict = MS.fromList $ zip (sort setToOrigin) [1..]
        insertNeighbors [ui, uj] (matrix, facets)
            | checkForNeighbors == Nothing = (matrix,facets)
            | otherwise = ((setElem 1 (i,j)) $ (setElem 1 (j,i)) matrix , (fromJust checkForNeighbors):facets)
            where
                i = fromJust $ MS.lookup ui dict
                j = fromJust $ MS.lookup uj dict
                checkForNeighbors = areNeighbors ui uj dict


-- indexedFacetToMatrix :: [FacetIndexed] -> Int -> Facets
-- indexedFacetToMatrix facets dimension = finalMatrix
--     where
--         initialMatrix = zero (length facets) dimension
--         ll@(finalMatrix,_) = foldr (\facet (matrix, i) -> ((foldr (\j mat-> setElem True (i,j) mat) matrix facet), i+1)) (initialMatrix,1) facets
-- computeHyperplane ::
--     [[Ratio Int]] ->    -- set of d d-dimensional vertices.Thus the matrix is square
--     [Ratio Int]         -- hyperplane
-- computeHyperplane vertices = NLA.toList $ (NLA.fromLists vertices) NLA.<\> (NLA.fromList (replicate (length vertices) 1)) 



computeHyperplane ::
    [Vertex] ->    -- set of d d-dimensional vertices.Thus the matrix is square
    Hyperplane         -- hyperplane
computeHyperplane vertices =  V.toList $ fromJust $ solveLS matrix vector
-- trace ("\n\nVERTICES: " ++ show vertices)
    where 
        matrix = fromLists vertices
        vector = V.fromList $ (replicate (length vertices) 1)

facetsToVertices :: [Facet] -> MS.Map Int Vertex -> [[Vertex]]
facetsToVertices facets dictIndexVertex = map (map (fromJust . (flip MS.lookup dictIndexVertex))) facets

generateBranches :: Int -> Int -> AdjacencyMatrix -> [Branch]
generateBranches idx dim adjacency = concatMap (goDeep adjacency dim) (map ((++[idx]).return) neighbors) 
    where
        neighbors = [col | col <- [1..(ncols adjacency)], getElem idx col adjacency]

goDeep :: AdjacencyMatrix -> Int -> Branch -> [Branch]
goDeep adjacency dim b@(x:xs)
    | length b == dim = return b
    | otherwise = concatMap (goDeep adjacency dim) $ ((delete []).nub) $ map (smartAppend b) neighbors 
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
    ([Facet],[Hyperplane])       -- set of hyperplanes ([[a]], [a])
facetEnumeration vertices =  (,) newFacets newHyperplanes
-- trace ("\n\nINITIAL FACETS: " ++ show facets ++ "\n\nINITIAL HYPER: " ++ show initialHyperplanes)
    where
        uSet = toOrigin vertices
        (adjacency, facets) = generateMatrices uSet
        dictVertexIndex = MS.fromList $ zip (sort uSet) [1..]
        dictIndexVertex = MS.fromList $ zip [1..] (sort uSet)
        initialHyperplanes = map computeHyperplane (facetsToVertices facets dictIndexVertex)
        (_,newFacets, newHyperplanes) = foldr (checkVertex dictIndexVertex dictVertexIndex) (adjacency,facets, initialHyperplanes) uSet


checkVertex ::
    MS.Map Int Vertex ->    -- ^ dictionany of indices and vertices
    MS.Map Vertex Int ->    -- ^ dictionany of vertices and indices
    Vertex ->               -- ^ vertex to check
    (AdjacencyMatrix,       -- ^ adjacency matrix 
    [Facet],                 -- ^ set of facets
    [Hyperplane]) ->        -- ^ set of hyperplanes
    (AdjacencyMatrix, [Facet], [Hyperplane])  -- ^ resulting tuple (adjacency,facets, hyperplanes)
checkVertex dictIndexVertex dictVertexIndex ui (adjacency, facets, hyperplanes) = joinTuple adjacency $ foldr (checkBranch dictIndexVertex ui) (facets, hyperplanes) branches
    where
        idx = fromJust $ MS.lookup ui dictVertexIndex
        dim = length ui
        facetsUi = [facet | facet <- facets, elem idx facet] -- take facets that include vertex ui
        --neighborsUi = [MS.lookup index dictIndexVertex | index <- [1..(length adjacency)] , (adjacency!!ui)!!index == 1 ]
        inFacets = \branch -> any (isSubsequenceOf (sort branch)) facetsUi
        branches = filter (not.inFacets) (generateBranches idx dim adjacency)
        joinTuple a (b,c) = (a,b,c)



checkBranch ::
    MS.Map Int Vertex ->    -- ^ dictionany of indices and vertices
    Vertex ->          -- ^ vertex ui that will be the root
    Branch ->   -- ^ branch under ui
    ([Facet],          -- ^ set of facets
    [Hyperplane]) ->       -- ^ set of hyperplanes
    ([Facet], [Hyperplane])  -- ^ resulting tuple (facets, hyperplanes)
checkBranch dictIndexVertex ui branch (facets, hyperplanes) =
    if (not.(elem hyper)) hyperplanes && all (<=1) equation12 && dot hyper ui > 0
    then (branch:facets,hyper:hyperplanes)
    else (facets,hyperplanes)
    where
        branchToVertices = map (\idx -> fromJust $ MS.lookup idx dictIndexVertex) branch
        hyper = computeHyperplane branchToVertices
        equation12 = map (dot hyper) branchToVertices


