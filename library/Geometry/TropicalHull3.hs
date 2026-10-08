-- | Tailored hull route for the three-variable graph one-skeleton.
--
-- This is the generalized hypersurface pipeline of the historical
-- @generalTropHyp@ branch rebuilt on the current fixed-arity modules:
-- 'Geometry.Vertex.extremalVertices' (GLPK extreme-point filtering) and
-- 'Geometry.Facet.facetEnumeration'' (Yang's facet enumeration) on the lifted
-- integer support, then the same enumeration on each projected lower cell. It
-- mirrors the comparison executable's reconstructed adapter, including its
-- corrected graph assembly. Only the cells and their facets come from this
-- pipeline; 'Geometry.TropicalGraph3.graph3FromCells' recomputes all dual
-- geometry exactly.
--
-- Coefficients must be integral and every coordinate must fit the legacy
-- 'Int' range; fractional input is rejected rather than routed elsewhere.
module Geometry.TropicalHull3 ( hullGraph3 ) where

import Data.List (foldl', nub, sort)
import Data.Ratio (denominator, numerator)
import Geometry.Facet (facetEnumeration')
import Geometry.TropicalGraph3 (Graph3, graph3FromCells, prepareTerms3)
import Geometry.TropicalSlice (Term3(..))
import Geometry.Vertex (extremalVertices)

type Point3 = [Integer]
type Point4 = [Integer]
type Cell = [Point3]
type Plane = ([Rational], Rational)

-- | Compute the dual-aware graph record through the tailored hull pipeline.
hullGraph3 :: [Term3] -> Either String Graph3
hullGraph3 input = do
    terms <- prepareTerms3 "hullGraph3" input
    mapM_ checkIntegralCoefficient terms
    cells <- originalCells terms
    facets <- traverse legacyCellFacets cells
    graph3FromCells "hullGraph3" terms (zip cells facets)

checkIntegralCoefficient :: Term3 -> Either String ()
checkIntegralCoefficient term
    | denominator (term3Coefficient term) /= 1 =
        Left "hullGraph3: the tailored hull route requires integral coefficients"
    | otherwise = mapM_ checkInt (termCoordinates term ++ [numerator (term3Coefficient term)])
  where
    checkInt n
        | n < toInteger (minBound :: Int) || n > toInteger (maxBound :: Int) =
            Left "hullGraph3: the tailored hull route requires coordinates within the legacy Int range"
        | otherwise = Right ()

termCoordinates :: Term3 -> [Integer]
termCoordinates term = [term3X term, term3Y term, term3Z term]

lifted :: Term3 -> Point4
lifted term = termCoordinates term ++ [numerator (term3Coefficient term)]

-- Lower facets use the generalized branch's incidence convention: a lower
-- support plane has an outward normal with negative coefficient component.
originalCells :: [Term3] -> Either String [Cell]
originalCells terms
    | rank (differences liftedPoints) == 3 =
        let extreme = sort . nub $ extremalVertices liftedPoints
        in if null extreme then Left "hullGraph3: no lifted vertices were found"
           else Right [map init extreme]
    | rank (differences liftedPoints) /= 4 =
        Left "hullGraph3: lifted support has unexpected affine dimension"
    | otherwise = do
        let extreme = sort . nub $ extremalVertices liftedPoints
        if null extreme then Left "hullGraph3: no lifted vertices were found" else pure ()
        oriented <- traverse (orientPlane liftedPoints) (facetEnumeration' extreme)
        let cells = [map init face | (face,(normal,_)) <- oriented, last normal < 0]
        if null cells
            then Left "hullGraph3: no lower lifted facets were found"
            else Right (sort . nub $ map (sort . nub) cells)
  where
    liftedPoints = map lifted terms

orientPlane :: [Point4] -> ([Point4], [Rational], Rational) -> Either String ([Point4], Plane)
orientPlane points (face, rawNormal, rawBound)
    | null face = Left "hullGraph3: facet enumeration returned an empty incidence set"
    | length rawNormal /= 4 = Left "hullGraph3: facet enumeration returned a malformed normal"
    | otherwise = do
        plane <- orientSupport points (rawNormal,rawBound)
        pure (face,plane)

-- Facet enumeration of one projected cell, restricted to the cell's own
-- extreme points, with every reported incidence rechecked on its plane.
legacyCellFacets :: Cell -> Either String [(Cell,[Rational])]
legacyCellFacets cell
    | rank (differences cell) /= 3 = Left "hullGraph3: a projected cell is not full-dimensional"
    | otherwise = do
        let extreme = sort . nub $ cell
        if length extreme < 4 then Left "hullGraph3: a projected cell has too few extreme points" else pure ()
        facets <- traverse (orientCellFacet extreme) (facetEnumeration' extreme)
        pure [(face,normal) | (face,normal,_) <- facets]

orientCellFacet :: Cell -> (Cell,[Rational],Rational) -> Either String (Cell,[Rational],Rational)
orientCellFacet allPoints (face,normal,bound)
    | null face = Left "hullGraph3: projected-cell facet enumeration returned an empty incidence set"
    | length normal /= 3 = Left "hullGraph3: projected-cell facet enumeration returned a malformed normal"
    | otherwise = do
        (h,b) <- orientSupport allPoints (normal,bound)
        if length face < 3 || any (\p -> dot h (map fromInteger p) /= b) face
            then Left "hullGraph3: a projected-cell facet incidence does not lie on its reported plane"
            else pure (sort . nub $ face,h,b)

orientSupport :: [[Integer]] -> Plane -> Either String Plane
orientSupport points (normal,bound)
    | any (> 0) sides && any (< 0) sides =
        Left "hullGraph3: a reported facet plane has input points on both sides"
    | all (<= 0) sides = Right (normalizePlane (normal,bound))
    | all (>= 0) sides = Right (normalizePlane (map negate normal,negate bound))
    | otherwise = Left "hullGraph3: could not orient a facet support plane"
  where
    sides = [dot normal (map fromInteger p)-bound | p <- points]

normalizePlane :: Plane -> Plane
normalizePlane (normal,bound) = (map fromInteger (init normalized), fromInteger (last normalized))
  where
    allEntries = normal ++ [bound]
    scale = foldl' lcm 1 (map denominator allEntries)
    integers = map (numerator . (* fromInteger scale)) allEntries
    divisor = max 1 (foldl' gcd 0 (map abs integers))
    normalized = map (`div` divisor) integers

dot :: [Rational] -> [Rational] -> Rational
dot a b = sum (zipWith (*) a b)

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
