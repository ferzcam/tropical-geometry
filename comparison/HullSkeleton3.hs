-- | Comparable adapters for the 3-variable tropical hypersurface graph
-- (vertices, bounded edges, and unbounded rays) returned by the generalized
-- 'origin/generalTropHyp' Polynomial.Hypersurface.hypersurface implementation.
-- This is the one-skeleton only, not a representation of the full tropical
-- surface. The original adapter follows that branch's extremalVertices ->
-- facetEnumeration' lifted-hull pipeline, reconstructed against current
-- fixed-arity modules (not a call to the old-branch executable); both routes
-- share only exact dual assembly. The LRS adapter independently uses
-- Geometry.LRSHull for lifted-hull and projected-cell facet enumeration.
module HullSkeleton3
    ( originalSkeleton3, lrsSkeleton3 ) where

import Data.List (foldl', groupBy, nub, sort, sortOn)
import Data.Ratio (denominator, numerator)
import Geometry.Facet (facetEnumeration')
import qualified Geometry.LRSHull as LRSHull
import Geometry.TropicalSlice (Term3(..))
import Geometry.Vertex (extremalVertices)
import Skeleton3 (Edge3(..), Skeleton3, canonicalSkeleton3)

-- A projected exponent point, and its lifted coefficient point.
type Point3 = [Integer]
type Point4 = [Integer]
type Cell = [Point3]
type Plane = ([Rational], Rational)
type CellInfo = ([Rational], [(Cell, [Integer])])

-- Normalize duplicate monomials by retaining their least min-plus coefficient.
normalize :: [Term3] -> Either String [Term3]
normalize input
    | null input = Left "A nonempty three-variable polynomial is required."
    | otherwise = Right $ map (head . sortOn term3Coefficient)
        (groupBy sameExponent (sortOn key input))
  where
    key t = (term3X t, term3Y t, term3Z t, term3Coefficient t)
    sameExponent a b = exponent a == exponent b
    exponent t = (term3X t, term3Y t, term3Z t)

checkIntegralCoefficient :: Term3 -> Either String ()
checkIntegralCoefficient term
    | denominator (term3Coefficient term) /= 1 =
        Left "The original generalized hypersurface route requires integral coefficients."
    | otherwise = do
        mapM_ checkInt (termCoordinates term ++ [numerator (term3Coefficient term)])
        pure ()
  where
    checkInt n
        | n < toInteger (minBound :: Int) || n > toInteger (maxBound :: Int) =
            Left "The original generalized hypersurface route requires coordinates within the legacy Int range."
        | otherwise = Right ()

termCoordinates :: Term3 -> [Integer]
termCoordinates term = [term3X term, term3Y term, term3Z term]

lifted :: Term3 -> Point4
lifted term = termCoordinates term ++ [numerator (term3Coefficient term)]

liftedRational :: Term3 -> [Rational]
liftedRational term = map fromInteger (termCoordinates term) ++ [term3Coefficient term]

-- The legacy pipeline computes lower facets using the same exact incidence
-- convention as the generalized branch: a lower support has outward normal
-- with negative coefficient-coordinate component.
originalCells :: [Term3] -> Either String [Cell]
originalCells terms
    | rank (differences exponents) /= 3 =
        Left "The original generalized graph adapter requires full-dimensional exponent support."
    | rank (differences liftedPoints) == 3 =
        let extreme = sort . nub $ extremalVertices liftedPoints
        in if null extreme then Left "Original generalized route found no lifted vertices."
           else Right [map init extreme]
    | rank (differences liftedPoints) /= 4 =
        Left "Lifted support has unexpected affine dimension."
    | otherwise = do
        let extreme = sort . nub $ extremalVertices liftedPoints
        if null extreme then Left "Original generalized route found no lifted vertices." else pure ()
        oriented <- traverse (orientPlane liftedPoints) (facetEnumeration' extreme)
        let cells = [map init face | (face,(normal,_)) <- oriented,last normal < 0]
        if null cells
            then Left "Original generalized route found no lower lifted facets."
            else Right (sort . nub $ map (sort . nub) cells)
  where
    exponents = map termCoordinates terms
    liftedPoints = map lifted terms

-- Follow the old branch's global extremal-vertex / facet-enumeration route,
-- preserving each lower facet's original corner incidence. The exact support
-- check only verifies the returned plane; it does not re-filter corners.
orientPlane :: [Point4] -> ([Point4], [Rational], Rational)
            -> Either String ([Point4], Plane)
orientPlane points (face, rawNormal, rawBound)
    | null face = Left "Original facet enumeration returned an empty incidence set."
    | length rawNormal /= 4 = Left "Original facet enumeration returned a malformed normal."
    | otherwise = do
        plane <- orientSupport points (rawNormal,rawBound)
        pure (face,plane)

-- LRS polar enumeration is independent of the Yang/GLPK route above.
lrsCells :: [Term3] -> Either String [Cell]
lrsCells terms
    | rank (differences exponents) /= 3 =
        Left "The LRS graph adapter requires full-dimensional exponent support."
    | affineRank (map liftedRational terms) == 3 = Right [exponents]
    | affineRank (map liftedRational terms) /= 4 =
        Left "Lifted support has unexpected affine dimension."
    | otherwise = do
        let liftedPoints = map liftedRational terms
        facets <- LRSHull.lrsHull liftedPoints
        let extreme = [p | p <- liftedPoints,
                           rank [normal | (normal,bound) <- facets,
                                          dot normal p == bound] == 4]
            lower = [(normal,bound) | (normal,bound) <- facets, last normal < 0]
            cells = [ [map numerator (init p) | p <- extreme, dot normal p == bound]
                    | (normal,bound) <- lower ]
        if null cells
            then Left "LRS found no lower lifted facets."
            else Right (sort . nub $ map (sort . nub) cells)
  where
    exponents = map termCoordinates terms

-- Turn each lower cell into a tropical vertex and the rays dual to its
-- projected 3D facet normals. Matching facet incidence gives bounded edges.
assemble :: ([Term3] -> Either String [Cell])
         -> (Cell -> Either String [(Cell,[Rational])])
         -> [Term3] -> Either String Skeleton3
assemble cellRoute facetRoute terms = do
    cells <- cellRoute terms
    infos <- traverse (cellInfo terms facetRoute) cells
    let allFacets = [(facet,vertex,direction) |
                     (vertex,facets) <- infos,
                     (facet,direction) <- facets]
        groups = groupBy sameFacet $ sortOn (\(f,_,_) -> f) allFacets
    edges <- fmap concat . traverse makeEdge $ groups
    pure (canonicalSkeleton3 edges)
  where
    sameFacet (a,_,_) (b,_,_) = a == b
    makeEdge group = case group of
        [(facet,v,direction)] -> Right [Ray3 v direction]
        [(_,a,_),(_,b,_)]
            | a == b -> Left "Two adjacent subdivision cells have identical dual vertices."
            | otherwise -> Right [Segment3 (min a b) (max a b)]
        _ -> Left "A projected subdivision facet is incident to more than two cells."

cellInfo :: [Term3] -> (Cell -> Either String [(Cell,[Rational])])
         -> Cell -> Either String CellInfo
cellInfo terms facetRoute cell
    | rank (differences cell) /= 3 =
        Left "A lower projected cell is not full-dimensional; its dual face is not an isolated graph vertex."
    | otherwise = do
        vertex <- tropicalVertex terms cell
        facets <- facetRoute cell
        pure (vertex, [(sort . nub $ incidence, primitiveDirection normal)
                       | (incidence,normal) <- facets])

-- Fit the cell's exact affine coefficient function c(p)=a.p+d. The tropical
-- dual vertex is -a. This is independent of the lower-hull adapter.
tropicalVertex :: [Term3] -> Cell -> Either String [Rational]
tropicalVertex terms cell = do
    let rows = [map fromInteger p ++ [1] | p <- cell]
        rhs = [coefficientAt terms p | p <- cell]
    case solveLinear rows rhs of
        Nothing -> Left "Could not solve the exact affine function of a lower cell."
        Just [a,b,c,_] ->
            let vertex = map negate [a,b,c]
                active = [coefficientAt terms p + dot vertex (map fromInteger p) | p <- cell]
                allValues = [term3Coefficient t + dot vertex (map fromInteger (termCoordinates t))
                            | t <- terms]
            in if null active || any (/= head active) active || head active /= minimum allValues
               then Left "A projected cell is not an exact lower face of the normalized polynomial."
               else Right vertex
        Just _ -> Left "Affine cell solve returned an unexpected dimension."

coefficientAt :: [Term3] -> Point3 -> Rational
coefficientAt terms point = case [term3Coefficient t | t <- terms,
                                    termCoordinates t == point] of
    [coefficient] -> coefficient
    [] -> error "coefficientAt: lower-cell exponent is absent from normalized support"
    _ -> error "coefficientAt: duplicate normalized exponent"

-- Original generalized branch's projected-cell facet enumeration, with facet
-- incidences restricted to the original route's extremal cell points.
legacyCellFacets :: Cell -> Either String [(Cell,[Rational])]
legacyCellFacets cell
    | rank (differences cell) /= 3 = Left "Projected cell facet enumeration requires dimension three."
    | otherwise = do
        -- These are already the global lifted-facet corners, exactly as in
        -- generalTropHyp's projected-cell facetEnumeration' call.
        let extreme = sort . nub $ cell
        if length extreme < 4 then Left "Projected cell has too few extreme points." else pure ()
        facets <- traverse (orientCellFacet extreme) (facetEnumeration' extreme)
        pure [(face,normal) | (face,normal,_) <- facets]

orientCellFacet :: Cell -> (Cell,[Rational],Rational)
                -> Either String (Cell,[Rational],Rational)
orientCellFacet allPoints (face,normal,bound)
    | null face = Left "Projected-cell facet enumeration returned an empty incidence set."
    | length normal /= 3 = Left "Projected-cell facet enumeration returned a malformed normal."
    | otherwise = do
        (h,b) <- orientSupport allPoints (normal,bound)
        if length face < 3 || any (\p -> dot h (map fromInteger p) /= b) face
            then Left "Projected-cell facet incidence does not lie on its reported plane."
            else pure (sort . nub $ face,h,b)

-- Independent LRS projected-cell facet route. Determine the actual cell
-- corners by requiring three independent active outward facet normals, then
-- match subdivision faces by their corner sets rather than all tied support.
lrsCellFacets :: Cell -> Either String [(Cell,[Rational])]
lrsCellFacets cell
    | rank (differences cell) /= 3 = Left "LRS projected-cell facet enumeration requires dimension three."
    | otherwise = do
        planes <- LRSHull.lrsHull (map (map fromInteger) cell)
        let extreme = [p | p <- cell,
                           rank [normal | (normal,bound) <- planes,
                                   dot normal (map fromInteger p) == bound] == 3]
        if length extreme < 4 then Left "LRS found too few projected-cell vertices." else pure ()
        traverse (oneFacet extreme) planes
  where
    oneFacet extreme plane@(normal,bound) = do
        (h,b) <- orientSupport cell plane
        let corners = [p | p <- extreme, dot h (map fromInteger p) == b]
        if length corners < 3
            then Left "LRS projected-cell facet has fewer than three extreme corners."
            else Right (sort (nub corners),h)

orientSupport :: [[Integer]] -> Plane -> Either String Plane
orientSupport points (normal,bound)
    | any (> 0) sides && any (< 0) sides =
        Left "A reported facet plane has input points on both sides."
    | all (<= 0) sides = Right (normalizePlane (normal,bound))
    | all (>= 0) sides = Right (normalizePlane (map negate normal,negate bound))
    | otherwise = Left "Could not orient a facet support plane."
  where
    sides = [dot normal (map fromInteger p)-bound | p <- points]

normalizePlane :: Plane -> Plane
normalizePlane (normal,bound) = (map fromInteger (init normalized), fromInteger (last normalized))
  where
    allEntries = normal ++ [bound]
    scale = foldl' lcm 1 (map denominator allEntries)
    integers = map (numerator . (* fromInteger scale)) allEntries
    divisor = foldl' gcd 0 (map abs integers)
    normalized = map (\n -> n `div` divisor) integers

primitiveDirection :: [Rational] -> [Integer]
primitiveDirection normal =
    let entries = map negate normal
        scale = foldl' lcm 1 (map denominator entries)
        integers = map (numerator . (* fromInteger scale)) entries
        divisor = foldl' gcd 0 (map abs integers)
    in map (`div` divisor) integers

dot :: [Rational] -> [Rational] -> Rational
dot a b = sum (zipWith (*) a b)

differences :: [[Integer]] -> [[Rational]]
differences [] = []
differences (origin:points) =
    [zipWith (\x y -> fromInteger (y-x)) origin point | point <- points]

affineRank :: [[Rational]] -> Int
affineRank [] = 0
affineRank (origin:points) = rank (map (zipWith (-) origin) points)

rank :: [[Rational]] -> Int
rank [] = 0
rank rows
    | null (head rows) = 0
    | otherwise = case break ((/= 0) . head) rows of
        (_,[]) -> rank (map tail rows)
        (before,pivot:after) -> 1 + rank
            [zipWith (-) (tail row) (map (* (head row / head pivot)) (tail pivot))
            | row <- before ++ after]

-- Exact Gauss-Jordan solve for a possibly overdetermined linear system.
solveLinear :: [[Rational]] -> [Rational] -> Maybe [Rational]
solveLinear rows rhs
    | null rows || length rows /= length rhs = Nothing
    | any ((/= n) . length) rows = Nothing
    | otherwise = eliminate (zipWith (++) rows (map (:[]) rhs)) 0 0
  where
    n = length (head rows)
    eliminate matrix row col
        | col == n = if any ((/= 0) . last) (drop row matrix) then Nothing
                     else Just (map last (take n matrix))
        | row == length matrix = if all ((== 0) . last) matrix then Nothing else Nothing
        | otherwise = case [i | (i,r) <- zip [row..] (drop row matrix), r !! col /= 0] of
            [] -> eliminate matrix row (col+1)
            (pivotIndex:_) ->
                let swapped = swapRows row pivotIndex matrix
                    pivot = map (/ (swapped !! row !! col)) (swapped !! row)
                    cleared = [if i == row then pivot else
                        zipWith (-) r (map (*(r !! col)) pivot)
                        | (i,r) <- zip [0::Int ..] swapped]
                in eliminate cleared (row+1) (col+1)
    swapRows i j xs = [pick k | k <- [0..length xs-1]]
      where pick k | k == i = xs !! j
                   | k == j = xs !! i
                   | otherwise = xs !! k

originalSkeleton3 :: [Term3] -> Either String Skeleton3
originalSkeleton3 input = do
    terms <- normalize input
    mapM_ checkIntegralCoefficient terms
    assemble originalCells legacyCellFacets terms

lrsSkeleton3 :: [Term3] -> Either String Skeleton3
lrsSkeleton3 input = do
    terms <- normalize input
    assemble lrsCells lrsCellFacets terms
