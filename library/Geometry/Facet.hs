module Geometry.Facet 


where

import Util
import Geometry.Vertex
import Geometry.LRS (colFromList)

import Data.Matrix hiding (trace)
import Data.List (sort, delete, nub, isSubsequenceOf, find, (\\))
import qualified Data.Map.Strict as MS
import Data.Maybe
import Debug.Trace
import Linear.Matrix (luSolve)
import Control.Lens
import  qualified Data.Vector as V


-- | Module that implements functions for facet enumeration. Based on the artcile written by Yaguang Yang: A Facet Enumeration Algorithm for Convex Polytopes. 


type Branch = [Int]
type Facet = [Int]
type Hyperplane = [Rational]


remove :: Eq a => a -> [a] -> [a]
remove elem = (delete elem).nub

centroid :: [IVertex] -> Vertex
centroid set = let 
                    n = fromIntegral $ length set 
                    fractionalSet = map (map toRational) set 
                in map (/n) (foldr1 (safeZipWith (+)) fractionalSet)

toOrigin :: [IVertex] -> [Vertex]
toOrigin set = let 
                    fractionalSet = map (map toRational) set 
                    center = centroid set
                in map ($-$ center) fractionalSet



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
    | otherwise =  Just $ map (detLU.fromLists.(diff ++).return) ident
    where
        dim = length (head vertices)
        points = take dim vertices
        diff = map ($-$ (head points)) (tail points)
        ident = toLists $ identity dim
        normalize vector = if (snd norm) == 0 then Nothing else Just $ map (/(snd norm)) vector
            where
                norm = foldl (\(pos, value) x -> (pos+1,value + ((-1)^pos)*x)) (1,0) vector 

-- computeHyperplane' ::
--     [Vertex] ->    -- set of d d-dimensional vertices.Thus the matrix is square
--     Maybe Hyperplane         -- hyperplane
-- computeHyperplane' [] = Nothing
-- computeHyperplane' vertices
--     | length vertices < dim = error "There must be at least d d-dimensional vertices for computing hyperplane"
--     | otherwise = Just $ map (detLU.fromLists.(diff ++).return) ident

--     where
--         dim = length (head vertices)
--         points = take dim vertices
--         diff = map ($-$ (head points)) (tail points)
--         ident = toLists $ identity dim

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



-- -- | Given a set of points of dimension d, we return a list of d points that include all the coordinates.
-- properVertices :: [Vertex] -> [Vertex]
-- properVertices vertices
--     | length vertices < dim = error "properVertices: not enough points"
--     | otherwise = niceVertices vertices (replicate dim False) [] 0
--     where
--         dim = length $ head vertices
--         niceVertices vertxs bools accVertxs pos
--             | and bools = accVertxs ++ (take (dim-(length accVertxs)) (reverse $ sort vertxs))
--             | pos >= dim = error "properVertices.niceVertices: dimension underflow"
--             | chosenPoint == Nothing && (bools!!pos) == False = error "properVertices.niceVertices: dimension underflow"
--             | otherwise = if bools!!pos then 
--                             niceVertices vertxs bools accVertxs (pos+1)
--                         else
--                             niceVertices (delete (fromJust chosenPoint) vertxs) newBools ((fromJust chosenPoint):accVertxs) (pos+1) 
--             where
--                 chosenPoint = find (\v -> v!!pos /= 0) vertxs
--                 varsEnabled = [i | i <- [0..(dim-1)], (fromJust chosenPoint)!!i /= 0]
--                 newBools = foldr (\i acc -> acc & element i .~ True) bools varsEnabled

isEmbedded :: [IVertex] -> Bool
isEmbedded vertices
    | all (==0) (map last vertices) = True 
    | length vertices <= dim = True
    | otherwise = all (\p -> detLU ((fromLists (p:firstD)) <|> e) == 0 ) rest
    where
        fractionalVertices =  map (map toRational) vertices
        dim = length $ head fractionalVertices
        firstD = take dim fractionalVertices
        rest = fractionalVertices\\firstD
        e = colFromList $ replicate (dim+1) 1


{- 
    facetEnumetation corresponds to algorithm 2.1 of the aforementioned paper.
    checkVertex corresponds to the outer loop 
    checkBranches corresponds to the inner loop

 -}
-- Exact affine-rank check. Lower-dimensional hulls need affine-hull
-- equalities in addition to their intrinsic supporting inequalities.
isLowerDimensional :: [IVertex] -> Bool
isLowerDimensional [] = False
isLowerDimensional (origin:points) =
    exactRowRank [map toRational (zipWith (-) point origin) | point <- points]
        < length origin

-- Gaussian elimination over Rational avoids tolerance decisions. Unlike
-- checking only the first d points, it finds independent rows anywhere.
exactRowRank :: [[Rational]] -> Int
exactRowRank [] = 0
exactRowRank rows
    | null (head rows) = 0
    | otherwise = case break ((/= 0) . head) rows of
        (_, []) -> exactRowRank (map tail rows)
        (before, pivot:after) -> 1 + exactRowRank
            [zipWith (-) (tail row)
                (map (* (head row / head pivot)) (tail pivot))
            | row <- before ++ after]

facetEnumeration :: 
    [IVertex] ->    -- set of vertices (not centered to origin)
    [(Facet, Hyperplane, Rational)]       -- set of hyperplanes ([[a]], [a], a)
facetEnumeration vertices
    | null vertices = error "facetEnumeration: empty input"
    | null (head vertices) = error "facetEnumeration: zero-dimensional ambient space"
    | any ((/= length (head vertices)) . length) vertices =
        error "facetEnumeration: inconsistent vertex dimensions"
    | length (head vertices) == 1 || isLowerDimensional vertices =
        affineFacetEnumeration vertices
    | otherwise = safeZipWith3 (,,) newFacets cleanedHypers b
    where
        uSet = sort $ toOrigin vertices
        center = centroid vertices
        adjacency = adjacencyMatrix uSet
        dictVertexIndex = MS.fromList $ zip uSet [1..]
        dictIndexVertex = MS.fromList $ zip [1..] uSet
        embedded = isEmbedded vertices
        (_,newFacets, newHyperplanes) = foldr (checkVertex dictIndexVertex dictVertexIndex embedded) (adjacency,[], []) uSet
        cleanedHypers = map fromJust $ remove Nothing newHyperplanes
        b = map (succ . (dot center)) cleanedHypers


facetEnumeration' :: 
    [IVertex] ->    -- set of vertices (not centered to origin)
    [([IVertex], Vertex, Rational)]       -- set of hyperplanes ([[a]], [a], a)
facetEnumeration' vertices =
    [(map (\i -> sort vertices !! (i-1)) ids,h,b)
    | (ids,h,b) <- facetEnumeration vertices]

-- | Ambient H-representation of a lower-dimensional convex hull. Select
-- independent original coordinates, so projection retains integral input.
-- Intrinsic facet IDs are remapped into the original sorted input. The final
-- pairs of inequalities encode affine-hull equalities; their incidence lists
-- contain every input point, not an intrinsic facet of the hull.
affineFacetEnumeration :: [IVertex] -> [(Facet, Hyperplane, Rational)]
affineFacetEnumeration vertices = intrinsic ++ equalities
    where
        original = sort vertices
        origin = map toRational (head original)
        dim = length origin
        differences = [zipWith (-) (map toRational v) origin | v <- original]
        columns = foldl addColumn [] [0..dim-1]
        addColumn selected j =
            let candidate = selected ++ [j]
            in if exactRowRank (map (project candidate) differences) > length selected
               then candidate else selected
        project indices row = map (row !!) indices
        rank = length columns
        projected = sort . nub $ map (project columns) original
        liftNormal h = [fromMaybe 0 (lookup j (zip columns h)) | j <- [0..dim-1]]
        -- Projection is injective on the affine hull, including all original
        -- support points; use exact incidence to retain duplicate input IDs.
        incidence h b = [i | (i,v) <- zip [1..] original,
                            dot h (map toRational v) == b]
        liftFacet (_,h,b) = let ambient = liftNormal h
                            in (incidence ambient b, ambient,b)
        intrinsic
            | rank == 0 = []
            | rank == 1 = map liftFacet
                [([],[-1],negate (toRational (head (head projected)))),
                 ([],[1],toRational (head (last projected)))]
            | otherwise = map liftFacet (facetEnumeration (extremalVertices projected))
        independent = foldl addRow [] differences
        addRow selected row
            | exactRowRank (map (project columns) (selected ++ [row])) > length selected = selected ++ [row]
            | otherwise = selected
        coefficients j
            | rank == 0 = []
            | otherwise = case solveLS (fromLists (map (project columns) independent))
                                    (V.fromList (map (!! j) independent)) of
                Just solution -> V.toList solution
                Nothing -> error "affineFacetEnumeration: independent coordinate solve failed"
        equation j =
            let coeffs = coefficients j
                normal = zipWith (-) [if k == j then 1 else 0 | k <- [0..dim-1]]
                                     (liftNormal coeffs)
                rhs = dot normal origin
                ids = [1..length original]
            in [(ids,normal,rhs),(ids,map negate normal,negate rhs)]
        equalities = concatMap equation ([0..dim-1] \\ columns)


checkVertex ::
    MS.Map Int Vertex ->    -- ^ dictionany of indices and vertices
    MS.Map Vertex Int ->    -- ^ dictionany of vertices and indices
    Bool ->                 -- ^ if the polyhedron has lower dimension than the ambient space
    Vertex ->               -- ^ vertex to check
    (AdjacencyMatrix,       -- ^ adjacency matrix 
    [Facet],                 -- ^ set of facets
    [Maybe Hyperplane]) ->        -- ^ set of hyperplanes
    (AdjacencyMatrix, [Facet], [Maybe Hyperplane])  -- ^ resulting tuple (adjacency,facets, hyperplanes)
checkVertex dictIndexVertex dictVertexIndex embedded ui (adjacency, facets, hyperplanes) = joinTuple adjacency $ foldr (checkBranch dictIndexVertex ui embedded) (facets, hyperplanes) branches
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
    Bool ->                 -- ^ if the polyhedron has lower dimension than the ambient space
    Branch ->   -- ^ branch under ui
    ([Facet],          -- ^ set of facets
    [Maybe Hyperplane]) ->       -- ^ set of hyperplanes
    ([Facet], [Maybe Hyperplane])  -- ^ resulting tuple (facets, hyperplanes)
checkBranch dictIndexVertex ui embedded branch (facets, hyperplanes)
    | hyper == Nothing = (facets, hyperplanes)
    | otherwise =
        if (not.(elem hyper)) hyperplanes && all (<= pure 1) equation12 && (((fmap dot hyper) <*> (pure ui)) > pure 0 || embedded)
        then ((complete branch):facets,hyper:hyperplanes)
        else  (facets,hyperplanes)
        where
            branchToVertices = map (\idx -> fromJust $ MS.lookup idx dictIndexVertex) branch
            isTrivialVal = all (==0) (map last branchToVertices) 
            (hyper, computedWithDet) = computeHyperplane branchToVertices
            set = map snd $ MS.toList dictIndexVertex
            equation12 = map (((fmap dot hyper) <*>) . pure) set
            complete br = if computedWithDet then
                                sortBySublist branch (MS.keys dictIndexVertex)
                        else sortBySublist branch (MS.keys $ MS.filter (\v -> ((fmap dot hyper) <*> (pure v)) == pure 1) dictIndexVertex)

                --sortBySublist branch (MS.keys $ MS.filter (\v -> ((fmap dot hyper) <*> (pure v)) == pure 1) dictIndexVertex)
--  if computedWithDet then
--                                 sortBySublist branch (MS.keys dictIndexVertex)
--                         else sortBySublist branch (MS.keys $ MS.filter (\v -> ((fmap dot hyper) <*> (pure v)) == pure 1) dictIndexVertex)



sortBySublist :: (Eq a) => [a] -> [a] -> [a]
sortBySublist [] bigList = bigList
sortBySublist (x:xs) bigList
    | elem x bigList = x : sortBySublist xs (delete x bigList)
    | otherwise = error "sortBySublist: element is not found in bigger list"
