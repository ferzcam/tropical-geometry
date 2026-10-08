-- | Tailored convex-hull route for two-variable tropical curves.
--
-- Integral terms are lifted to @(x, y, coefficient)@ points, the incremental
-- 'Geometry.ConvexHull3.convexHull3' builds their hull, and
-- 'Geometry.Polytope.projectionToR2' projects its lower faces to the regular
-- Newton subdivision. The subdivision is then dualized exactly by
-- 'Geometry.TropicalCurve.curveFromLowerCells', which recomputes and validates
-- every dual vertex, weight, and ray with rational arithmetic.
--
-- This route deliberately keeps the legacy hull contract: coefficients must be
-- integers within the machine 'Int' range, and the exponent support must have
-- affine dimension two. Other inputs are rejected; nothing falls back to the
-- direct or LRS solvers.
module Geometry.TropicalHull2 ( hullTropicalCurve ) where

import Data.Ratio (denominator, numerator)
import Geometry.ConvexHull3 (Point3D, convexHull3)
import Geometry.Polytope (projectionToR2)
import Geometry.TropicalCurve (Curve, Term(..), curveFromLowerCells, normalizeTerms)

-- | Compute the exact 'Curve' record through the tailored hull pipeline.
-- Duplicate exponents retain their least coefficient; term IDs index the
-- normalized, exponent-sorted terms as for the direct and LRS routes.
hullTropicalCurve :: [Term] -> Either String Curve
hullTropicalCurve input
    | null input = Left "hullTropicalCurve: at least one finite polynomial term is required."
    | length input > 64 = Left "hullTropicalCurve: at most 64 polynomial terms are supported."
    | length terms < 3 || rank (differences exponents) < 2 =
        Left "hullTropicalCurve: exponent support must have affine dimension two."
    | otherwise = do
        lifted <- traverse integerLift terms
        hull <- maybe (Left "hullTropicalCurve: the lifted hull is empty.") Right (convexHull3 lifted)
        let cells = map (map (\(x,y) -> (toInteger x,toInteger y))) (projectionToR2 hull)
        if null cells then Left "hullTropicalCurve: no lower subdivision cells were found." else pure ()
        curveFromLowerCells "hullTropicalCurve" terms cells
  where
    terms = normalizeTerms input
    exponents = [[termX t,termY t] | t <- terms]

integerLift :: Term -> Either String Point3D
integerLift term = do
    x <- boundedInt "x exponent" (termX term)
    y <- boundedInt "y exponent" (termY term)
    coefficient <- if denominator (termCoefficient term) == 1
        then boundedInt "coefficient" (numerator (termCoefficient term))
        else Left "hullTropicalCurve: the tailored hull route requires integral coefficients."
    pure (x,y,coefficient)

boundedInt :: String -> Integer -> Either String Int
boundedInt label value
    | value < toInteger (minBound :: Int) || value > toInteger (maxBound :: Int) =
        Left ("hullTropicalCurve: " ++ label ++ " is outside the legacy Int range.")
    | otherwise = Right (fromInteger value)

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
