-- | Exact ONE-SKELETON of a three-variable min-plus hypersurface together with
-- the dual entities of its regular Newton subdivision.
--
-- A graph vertex is dual to a three-dimensional subdivision cell; a graph edge
-- (bounded segment or unbounded ray) is dual to a two-dimensional subdivision
-- face. This record carries those incidences so a viewer can link the two
-- pictures. It is NOT the complete two-dimensional tropical surface: the
-- surface's two-dimensional pieces, dual to subdivision edges, are not
-- represented.
--
-- Three routes build the same record. 'exactGraph3' clips equality lines
-- directly ("Geometry.TropicalSkeleton3"), 'lrsGraph3' enumerates lifted and
-- projected hull facets with the Haskell LRS, and
-- 'Geometry.TropicalHull3.hullGraph3' uses the tailored GLPK/Yang hull
-- pipeline. Hull-based routes supply only lower cells and their facets;
-- 'graph3FromCells' recomputes and validates all dual data exactly.
module Geometry.TropicalGraph3
    ( GraphVertex3(..), GraphEdge3(..), SubdivisionCell3(..), SubdivisionFace3(..)
    , Graph3(..), exactGraph3, lrsGraph3, graph3FromCells, prepareTerms3
    , normalizeTerms3, skeleton3Of
    ) where

import Control.Monad (forM_, unless, when)
import Data.List (foldl', groupBy, nub, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Ratio (denominator, numerator)
import Geometry.TropicalSkeleton3
    ( Edge3(..), Skeleton3(..), canonicalSkeleton3, exactSkeleton3, lowerCells3
    , lrsCellFacets3 )
import Geometry.TropicalSlice (Term3(..))

-- | A vertex of the graph; 'vertex3Terms' are the term IDs attaining the
-- minimum there, which are exactly the terms of its dual cell.
data GraphVertex3 = GraphVertex3
    { vertex3Id :: Int, vertex3Point :: [Rational], vertex3Terms :: [Int]
    , vertex3Cell :: Int
    } deriving (Eq, Show)

-- | A segment or ray of the one-skeleton, its minimizing terms, its dual
-- face, and the IDs of its incident graph vertices.
data GraphEdge3 = GraphEdge3
    { edge3Id :: Int, edge3Geometry :: Edge3, edge3Terms :: [Int]
    , edge3Face :: Int, edge3Vertices :: [Int]
    } deriving (Eq, Show)

-- | A three-dimensional lower cell: its minimizing terms (including any
-- non-extreme support) and the IDs of its two-dimensional faces.
data SubdivisionCell3 = SubdivisionCell3
    { cell3Id :: Int, cell3Vertex :: Int, cell3Terms :: [Int], cell3Faces :: [Int]
    } deriving (Eq, Show)

-- | A two-dimensional lower face: its terms, its convex boundary as a cyclic
-- list of term IDs, the one or two cells containing it, and its dual edge.
data SubdivisionFace3 = SubdivisionFace3
    { face3Id :: Int, face3Terms :: [Int], face3Boundary :: [Int]
    , face3Cells :: [Int], face3Edge :: Int
    } deriving (Eq, Show)

-- | Normalized terms and all dual entities. Cell IDs equal their dual vertex
-- IDs and face IDs equal their dual edge IDs; the explicit fields keep the
-- correspondence self-describing.
data Graph3 = Graph3
    { graph3Terms :: [Term3], graph3Vertices :: [GraphVertex3]
    , graph3Edges :: [GraphEdge3], graph3Cells :: [SubdivisionCell3]
    , graph3Faces :: [SubdivisionFace3]
    } deriving (Eq, Show)

type Point3R = [Rational]
type Point3I = [Integer]

-- | Duplicate exponents retain their least coefficient; the result is sorted
-- by exponent so IDs are stable under input reordering.
normalizeTerms3 :: [Term3] -> [Term3]
normalizeTerms3 = map (head . sortOn term3Coefficient) . groupBy sameExponent . sort
  where
    sameExponent a b = exponentI a == exponentI b

-- | Shared input contract: nonempty, at most 32 terms, affine rank three.
prepareTerms3 :: String -> [Term3] -> Either String [Term3]
prepareTerms3 label input
    | null input = Left (label ++ ": nonempty finite polynomial required")
    | length input > 32 = Left (label ++ ": at most 32 terms are supported")
    | rank (differences (map exponentI terms)) /= 3 =
        Left (label ++ ": exponent support must have affine rank three")
    | otherwise = Right terms
  where
    terms = normalizeTerms3 input

-- | Direct route: the edges of 'exactSkeleton3' and the terms attaining the
-- minimum at vertices and along edge interiors define cells and faces.
exactGraph3 :: [Term3] -> Either String Graph3
exactGraph3 input = do
    terms <- prepareTerms3 "exactGraph3" input
    skeleton <- exactSkeleton3 terms
    let vertices = [(p,active3 terms p) | p <- vertices3 skeleton]
    faces <- traverse (faceOf terms) (edges3 skeleton)
    finishGraph3 "exactGraph3" terms vertices faces
  where
    faceOf terms edge = case edge of
        Line3 _ _ -> Left "exactGraph3: a complete line has no dual subdivision face"
        _ -> Right (active3 terms (interior3 edge),endpoints3 edge,edge)

-- | Independent LRS route: lower lifted-hull facets give the cells and the LRS
-- hull of each projected cell gives its faces.
lrsGraph3 :: [Term3] -> Either String Graph3
lrsGraph3 input = do
    terms <- prepareTerms3 "lrsGraph3" input
    cells <- lowerCells3 terms
    facets <- traverse lrsCellFacets3 cells
    graph3FromCells "lrsGraph3" terms (zip cells facets)

-- | Exact dualization shared by hull-based routes. Each cell is given by its
-- extreme exponent corners and its facets as (sorted corner set, outward
-- normal). Dual vertices are solved and validated against the global minimum,
-- faces are matched across cells by their corner sets, and every incidence is
-- rechecked; the terms must be the output of 'prepareTerms3'.
graph3FromCells :: String -> [Term3] -> [([Point3I],[([Point3I],[Rational])])] -> Either String Graph3
graph3FromCells label terms cells = do
    mapM_ checkClosed cells
    duals <- traverse dualVertex cells
    let incidence = Map.fromListWith (flip (++))
            [ (sort (nub corners),[(vertex,ids,normal)])
            | ((_,facets),(vertex,ids)) <- zip cells duals
            , (corners,normal) <- facets ]
    faces <- traverse faceFrom (Map.toList incidence)
    finishGraph3 label terms duals faces
  where
    failure message = Left (label ++ ": " ++ message)
    -- Completeness of a cell: every polygon edge of its facets must be shared
    -- by exactly two facets, otherwise the hull pipeline missed a facet.
    checkClosed (corners,facets) = do
        when (null facets) $ failure "a lower cell has no enumerated facets"
        cycles <- traverse (facetCycle corners) facets
        let counts = Map.fromListWith (+)
                [ (if a <= b then (a,b) else (b,a),1 :: Int)
                | cycle' <- cycles, (a,b) <- zip cycle' (tail cycle' ++ take 1 cycle') ]
        unless (all (== 2) (Map.elems counts)) $
            failure "the enumerated facets of a lower cell do not close its boundary; the hull pipeline missed a facet"
    facetCycle corners (fc,normal) = do
        when (any (`notElem` corners) fc) $ failure "a facet corner is not a corner of its cell"
        let dropped = head ([k | (k,c) <- zip [0 :: Int ..] normal, c /= 0] ++ [0])
            kept = filter (/= dropped) [0,1,2]
            cycle' = convexCycle [(i,(fromInteger (p !! head kept),fromInteger (p !! last kept))) | (i,p) <- zip [0..] fc]
        when (length cycle' /= length fc || length fc < 3) $
            failure "a facet of a lower cell is not a convex polygon of its corners"
        pure (map (fc !!) cycle')
    dualVertex (corners,_) = do
        when (rank (differences corners) /= 3) $
            failure "a lower cell is not full-dimensional"
        vertex <- solveVertex corners
        let ids = active3 terms vertex
            minimal = [exponentI (terms !! i) | i <- ids]
        when (any (`notElem` minimal) corners) $
            failure "a lower cell corner does not attain the minimum at its dual vertex"
        pure (vertex,ids)
    solveVertex corners = do
        rows <- traverse row corners
        case solveAffine rows of
            Nothing -> failure "could not solve the exact affine function of a lower cell"
            Just gradient -> do
                let vertex = map negate gradient
                    values = [value3 t vertex | t <- terms]
                    cornerValues = [c + dot vertex (map fromInteger p) | (p,c) <- rows]
                when (any (/= minimum values) cornerValues) $
                    failure "a lower cell fails exact global-minimum validation"
                pure vertex
    row corner = case [term3Coefficient t | t <- terms, exponentI t == corner] of
        [c] -> Right (corner,c)
        _ -> failure "a lower cell corner is not a normalized exponent"
    -- Completeness across cells: a facet with one enumerated cell must support
    -- the entire exponent set, otherwise the cell on its far side is missing.
    faceFrom (corners,occurrences) = case occurrences of
        [(vertex,ids,normal)]
            | any (\t -> dot normal (exponentR t) > dot normal (map fromInteger (head corners))) terms ->
                failure "exponents lie beyond a face with only one enumerated cell; the hull pipeline missed a lower cell"
            | otherwise ->
                Right (onPlane ids corners normal,[vertex],Ray3 vertex (primitive (map negate normal)))
        [(a,idsA,normalA),(b,idsB,normalB)]
            | a == b -> failure "adjacent cells have identical dual vertices"
            | onPlane idsA corners normalA /= onPlane idsB corners normalB ->
                failure "adjacent cells disagree on the terms of a shared face"
            | otherwise -> Right (onPlane idsA corners normalA,[a,b],Segment3 (min a b) (max a b))
        _ -> failure "a projected facet is incident to more than two cells"
    onPlane ids corners normal =
        let bound = dot normal (map fromInteger (head corners))
        in [i | i <- ids, dot normal (exponentR (terms !! i)) == bound]

-- Assemble, sort, and validate the record from dual vertices (point, cell
-- terms) and faces (terms, incident vertex points, dual edge geometry).
finishGraph3 :: String -> [Term3] -> [(Point3R,[Int])] -> [([Int],[Point3R],Edge3)]
             -> Either String Graph3
finishGraph3 label terms rawVertices rawFaces = do
    let points = sort (map fst rawVertices)
    when (length points /= length (nub points)) $ failure "duplicate dual vertices"
    let vertexIds = Map.fromList (zip points [0..])
        cellTerms = Map.fromList rawVertices
        vertices = [GraphVertex3 i p (cellTerms Map.! p) i | (i,p) <- zip [0..] points]
        ordered = sortOn (\(_,_,g) -> canonicalEdge g) rawFaces
        geometries = [canonicalEdge g | (_,_,g) <- ordered]
    when (length geometries /= length (nub geometries)) $ failure "duplicate dual edges"
    built <- traverse (buildFace vertexIds cellTerms) (zip [0..] ordered)
    let faces = map snd built
        edges = map fst built
        cells = [SubdivisionCell3 i i ids [face3Id f | f <- faces, i `elem` face3Cells f]
                | GraphVertex3 i _ ids _ <- vertices]
    forM_ cells $ \cell -> do
        when (rank (differences [exponentI (terms !! i) | i <- cell3Terms cell]) /= 3) $
            failure "a dual cell has rank-deficient support"
        when (length (cell3Faces cell) < 4) $ failure "a dual cell has fewer than four faces"
    pure (Graph3 terms vertices edges cells faces)
  where
    failure message = Left (label ++ ": " ++ message)
    buildFace vertexIds cellTerms (i,(ids,incident,geometry)) = do
        incidentIds <- traverse (\p -> maybe (failure "an edge endpoint is not a graph vertex") Right
                                        (Map.lookup p vertexIds)) incident
        forM_ incident $ \p ->
            unless (all (`elem` (cellTerms Map.! p)) ids) $
                failure "a face lists a term that its cell does not attain"
        let exponents = [exponentI (terms !! j) | j <- ids]
            diffs = differences exponents
            canonical = canonicalEdge geometry
            direction = edgeDirection canonical
        when (rank diffs /= 2) $ failure "a dual face does not have affine rank two"
        unless (all (\u -> dot u (map fromInteger direction) == 0) diffs) $
            failure "a dual face is not perpendicular to its edge"
        let normal = faceNormal diffs
            dropped = head [k | (k,c) <- zip [0 :: Int ..] normal, c /= 0]
            kept = filter (/= dropped) [0,1,2]
            projected = [(j,(fromInteger (e !! head kept),fromInteger (e !! last kept)))
                        | (j,e) <- zip ids exponents]
            boundary = convexCycle projected
        when (length boundary < 3) $ failure "a dual face boundary has fewer than three corners"
        pure ( GraphEdge3 i canonical ids i (sort incidentIds)
             , SubdivisionFace3 i ids boundary (sort incidentIds) i )

-- | The plain one-skeleton of a graph record, for comparison with
-- 'exactSkeleton3' and 'lrsTropicalSkeleton3'.
skeleton3Of :: Graph3 -> Skeleton3
skeleton3Of = canonicalSkeleton3 . map edge3Geometry . graph3Edges

canonicalEdge :: Edge3 -> Edge3
canonicalEdge (Segment3 a b) = if a <= b then Segment3 a b else Segment3 b a
canonicalEdge (Ray3 p d) = Ray3 p (primitive (map fromInteger d))
canonicalEdge (Line3 p d) = Line3 p (primitive (map fromInteger d))

edgeDirection :: Edge3 -> [Integer]
edgeDirection (Segment3 a b) = primitive (zipWith (-) b a)
edgeDirection (Ray3 _ d) = d
edgeDirection (Line3 _ d) = d

endpoints3 :: Edge3 -> [Point3R]
endpoints3 (Segment3 a b) = [a,b]
endpoints3 (Ray3 a _) = [a]
endpoints3 (Line3 _ _) = []

interior3 :: Edge3 -> Point3R
interior3 (Segment3 a b) = zipWith (\x y -> (x+y)/2) a b
interior3 (Ray3 p d) = zipWith (\x y -> x+fromInteger y) p d
interior3 (Line3 p _) = p

-- Normal of a rank-two difference set: the first nonzero cross product.
faceNormal :: [[Rational]] -> [Rational]
faceNormal diffs = head ([n | u <- diffs, v <- diffs, let n = cross u v, any (/= 0) n] ++ [[1,0,0]])

-- Counterclockwise convex boundary of projected points in a plane, excluding
-- collinear and interior support, as 'Geometry.TropicalCurve' does in 2D.
convexCycle :: [(Int,(Rational,Rational))] -> [Int]
convexCycle items
    | length points < 3 = map fst points
    | otherwise = map fst (init lower ++ init upper)
  where
    points = sortOn (\(j,p) -> (p,j)) items
    lower = reverse (foldl' step [] points)
    upper = reverse (foldl' step [] (reverse points))
    step (b:a:rest) p | turn a b p <= 0 = step (a:rest) p
    step acc p = p:acc
    turn (_,(x,y)) (_,(u,v)) (_,(a,b)) = (u-x)*(b-y)-(v-y)*(a-x)

-- Exact least-squares-free solve of c(p) = g.p + d over corner rows; any
-- affinely independent four corners determine (g,d), then every row is checked.
solveAffine :: [(Point3I,Rational)] -> Maybe [Rational]
solveAffine rows = listToMaybe
    [ take 3 solution
    | basis <- combinations 4 rows
    , Just solution <- [solveSquare [map fromInteger p ++ [1] | (p,_) <- basis] (map snd basis)]
    , all (\(p,c) -> dot (take 3 solution) (map fromInteger p) + solution !! 3 == c) rows ]

solveSquare :: [[Rational]] -> [Rational] -> Maybe [Rational]
solveSquare [] [] = Just []
solveSquare rows rhs = do
    pivotIndex <- listToMaybe [i | (i,r) <- zip [0 :: Int ..] rows, head r /= 0]
    let pivot = rows !! pivotIndex
        value = rhs !! pivotIndex
        others = [(r,b) | (i,(r,b)) <- zip [0 :: Int ..] (zip rows rhs), i /= pivotIndex]
        reduce (r,b) = (zipWith (-) (tail r) (map (* (head r / head pivot)) (tail pivot)),
                        b - head r / head pivot * value)
        reduced = map reduce others
    rest <- solveSquare (map fst reduced) (map snd reduced)
    pure ((value - dot (tail pivot) rest) / head pivot : rest)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

exponentI :: Term3 -> Point3I
exponentI t = [term3X t,term3Y t,term3Z t]

exponentR :: Term3 -> Point3R
exponentR = map fromInteger . exponentI

value3 :: Term3 -> Point3R -> Rational
value3 t p = term3Coefficient t + dot (exponentR t) p

active3 :: [Term3] -> Point3R -> [Int]
active3 terms p = [i | (i,v) <- zip [0..] values, v == least]
  where
    values = map (`value3` p) terms
    least = minimum values

primitive :: [Rational] -> [Integer]
primitive values = map (`div` divisor) integers
  where
    scale = foldl' lcm 1 (map denominator values)
    integers = map (numerator . (* fromInteger scale)) values
    divisor = max 1 (foldl' gcd 0 (map abs integers))

dot :: [Rational] -> [Rational] -> Rational
dot a b = sum (zipWith (*) a b)

cross :: [Rational] -> [Rational] -> [Rational]
cross [a,b,c] [x,y,z] = [b*z-c*y,c*x-a*z,a*y-b*x]
cross _ _ = [0,0,0]

differences :: [[Integer]] -> [[Rational]]
differences [] = []
differences (origin:points) = [zipWith (\x y -> fromInteger (y-x)) origin point | point <- points]

rank :: [[Rational]] -> Int
rank [] = 0
rank rows
    | null (head rows) = 0
    | otherwise = case break ((/= 0) . head) rows of
        (_,[]) -> rank (map tail rows)
        (before,pivot:after) -> 1 + rank
            [zipWith (-) (tail row) (map (* (head row / head pivot)) (tail pivot))
            | row <- before ++ after]
