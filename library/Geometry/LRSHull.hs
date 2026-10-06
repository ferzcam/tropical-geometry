-- | Facets of a bounded convex hull through exact polar duality and LRS.
--
-- For an affinely spanning finite point set P, its mean c lies strictly inside
-- conv(P). The polar of conv(P)-c is { y | (p-c).y <= 1 for every p in P }.
-- It is bounded, and its vertices y correspond to the original supporting
-- inequalities y.p <= 1+y.c. This uses only the bounded contract of
-- Geometry.LRS.lrs; no general unbounded vertex/ray output is needed.
--
-- LRS requires a feasible starting vertex, not the interior origin. We find
-- one by exact independent tight-basis search. This preparation is deliberately
-- exposed separately so benchmarks can report its cost as well as total cost.
-- No tailored convex hull, adjacency oracle, GLPK, or Yang enumeration is used.
module Geometry.LRSHull
    ( HullFacet, PreparedHull, prepareHull, enumerateHull, lrsHull ) where

import Data.List (find, nub, sort)
import Data.Matrix (fromLists)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ratio (denominator, numerator)
import Geometry.LRS (colFromList, lrs)

-- | Outward inequality normal.point <= bound. All entries are integral
-- rationals with positive common scale removed; orientation is retained.
type HullFacet = ([Rational], Rational)

-- | (center, polar constraint rows, feasible polar vertex).
-- Plain lists allow complete normal-form forcing in benchmark preparation.
-- Construct using 'prepareHull'; 'enumerateHull' assumes these invariants.
type PreparedHull = ([Rational], [[Rational]], [Rational])

-- | Only full-dimensional point sets in ambient dimensions 2, 3, and 4 are
-- accepted. Rank-deficient input needs a separate intrinsic-coordinate adapter.
prepareHull :: [[Rational]] -> Either String PreparedHull
prepareHull input
    | null input = Left "lrsHull: empty input"
    | dimension < 2 || dimension > 4 = Left "lrsHull: ambient dimension must be 2, 3, or 4"
    | any ((/= dimension) . length) input = Left "lrsHull: inconsistent point dimensions"
    | rank differences < dimension = Left "lrsHull: input must have full affine dimension"
    | otherwise = case seed of
        Nothing -> Left "lrsHull: no feasible polar starting basis found"
        Just start -> Right (center, rows, start)
  where
    dimension = length (head input)
    points = sort (nub input)
    origin = head points
    differences = map (`subtractPoint` origin) (tail points)
    center = map (/ fromIntegral (length points))
        (foldl (zipWith (+)) (replicate dimension 0) points)
    rows = map (`subtractPoint` center) points
    candidates = mapMaybe (\basis -> solveSquare basis (replicate dimension 1))
        (combinations dimension rows)
    seed = find (\y -> all (\row -> dot row y <= 1) rows) candidates

-- | Enumerate and canonically normalize primal facet inequalities. Preparation
-- and complete enumeration must both be included in end-to-end hull timings.
enumerateHull :: PreparedHull -> [HullFacet]
enumerateHull (center, rows, start) = sort . nub $
    [canonical (y, 1 + dot y center)
    | y <- lrs (fromLists rows) (colFromList (replicate (length rows) 1)) start]

lrsHull :: [[Rational]] -> Either String [HullFacet]
lrsHull = fmap enumerateHull . prepareHull

subtractPoint :: [Rational] -> [Rational] -> [Rational]
subtractPoint = zipWith (-)

dot :: [Rational] -> [Rational] -> Rational
dot x y = sum (zipWith (*) x y)

canonical :: HullFacet -> HullFacet
canonical (normal,bound) = (init normalized,last normalized)
  where
    entries = normal ++ [bound]
    scale = foldl lcm 1 (map denominator entries)
    integers = map (numerator . (* fromInteger scale)) entries
    divisor = foldl gcd 0 (map abs integers)
    normalized = map (fromInteger . (`div` divisor)) integers

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

rank :: [[Rational]] -> Int
rank [] = 0
rank rows
    | null (head rows) = 0
    | otherwise = case break ((/= 0) . head) rows of
        (_,[]) -> rank (map tail rows)
        (before,pivot:after) -> 1 + rank
            [zipWith (-) (tail row) (map (* (head row / head pivot)) (tail pivot))
            | row <- before ++ after]

-- Exact Gaussian elimination with back substitution. A singular chosen basis
-- is skipped; feasibility is subsequently checked against every polar row.
solveSquare :: [[Rational]] -> [Rational] -> Maybe [Rational]
solveSquare [] [] = Just []
solveSquare rows rhs = do
    pivotIndex <- listToMaybe [i | (i,row) <- zip [0 :: Int ..] rows, head row /= 0]
    let pivot = rows !! pivotIndex
        value = rhs !! pivotIndex
        others = [(row,b) | (i,(row,b)) <- zip [0 :: Int ..] (zip rows rhs), i /= pivotIndex]
        reduce (row,b) = (zipWith (-) (tail row) (map (* (head row / head pivot)) (tail pivot)),
                          b - head row / head pivot * value)
        reduced = map reduce others
    rest <- solveSquare (map fst reduced) (map snd reduced)
    pure ((value - dot (tail pivot) rest) / head pivot : rest)
