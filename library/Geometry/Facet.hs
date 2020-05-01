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


remove :: Eq a => a -> [a] -> [a]
remove elem = (delete elem).nub

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
    | not $ isOneFace ui uj set = Nothing
    | zValue == replicate (length ui) 0 = Nothing
    | all (<=1) inHalfSpace = (fmap sort indices)
    | otherwise = Nothing
    where
        set = MS.keys dict
        dimension = length ui
        zValue = zeta ui uj
        hValue = hyper zValue
        inHalfSpace = map (dot hValue) set
        inFacet = filter (\x -> dot hValue x == 1) set
        indices =  case length inFacet of
                    2 -> Just []
                    d ->  if d >= dimension
                            then Just $ map (fromJust . (flip MS.lookup dict)) inFacet 
                            else Just []
        
            
            

-- | Generates the adjacency matrix of vertices and the initial facet matrix. The centroid is computed inside the function.
generateMatrices :: [Vertex] -> (AdjacencyMatrix, [Facet])
generateMatrices set = fmap (remove []) $ foldr insertNeighbors ((zero nPoints nPoints),[]) pairs
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



computeHyperplane ::
    [Vertex] ->    -- set of d d-dimensional vertices.Thus the matrix is square
    Maybe Hyperplane         -- hyperplane
computeHyperplane [] = Nothing
computeHyperplane vertices =  fmap V.toList $ solveLS matrix vector
--V.toList $ fromJust $
-- trace ("\n\nVERTICES: " ++ show vertices)
    where 
        matrix = fromLists vertices
        vector = V.fromList $ (replicate (length vertices) 1)

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
    ([Facet],[Maybe Hyperplane])       -- set of hyperplanes ([[a]], [a])
facetEnumeration vertices =  (,) newFacets (remove Nothing $ newHyperplanes)
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
    [Maybe Hyperplane]) ->        -- ^ set of hyperplanes
    (AdjacencyMatrix, [Facet], [Maybe Hyperplane])  -- ^ resulting tuple (adjacency,facets, hyperplanes)
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
    MS.Map Int Vertex ->    -- ^ dictionary of indices and vertices
    Vertex ->          -- ^ vertex ui that will be the root
    Branch ->   -- ^ branch under ui
    ([Facet],          -- ^ set of facets
    [Maybe Hyperplane]) ->       -- ^ set of hyperplanes
    ([Facet], [Maybe Hyperplane])  -- ^ resulting tuple (facets, hyperplanes)
checkBranch dictIndexVertex ui branch (facets, hyperplanes) =
    if (not.(elem hyper)) hyperplanes && all (<= pure 1) equation12 && ((fmap dot hyper) <*> (pure ui)) > pure 0
    then (branch:facets,hyper:hyperplanes)
    else (facets,hyperplanes)
    where
        branchToVertices = map (\idx -> fromJust $ MS.lookup idx dictIndexVertex) branch
        hyper = computeHyperplane branchToVertices
        set = map snd $ MS.toList dictIndexVertex
        equation12 = map (((fmap dot hyper) <*>) . pure) set


