-- | Exact horizontal slices of finite min-plus polynomials in three variables.
--
-- A slice has two parts: the one-dimensional root locus of the specialized
-- polynomial, and any two-dimensional faces where distinct original terms
-- with the same projected exponent tie at the projected minimum.
module Geometry.TropicalSlice
    ( Term3(..), SliceInequality(..), SliceRegion(..), SliceResult(..)
    , tropicalSlice
    ) where

import Data.List (groupBy, sort, sortOn)
import Geometry.TropicalCurve

data Term3 = Term3
    { term3X :: Integer, term3Y :: Integer, term3Z :: Integer
    , term3Coefficient :: Rational
    } deriving (Eq, Ord, Show)

data SliceInequality = SliceInequality
    { inequalityA :: Integer, inequalityB :: Integer
    , inequalityBound :: Rational
    } deriving (Eq, Ord, Show)

data SliceRegion = SliceRegion
    { regionTerms :: [Int], regionInequalities :: [SliceInequality]
    } deriving (Eq, Show)

data SliceResult = SliceResult
    { sliceCurve :: Curve, sliceHeight :: Rational
    , sliceSourceTerms :: [Term3], sliceRegions :: [SliceRegion]
    } deriving (Eq, Show)

-- | Intersect the original 3D hypersurface with z = height. Duplicate 3D
-- exponents retain their least coefficient, matching the polynomial
-- convention. Source IDs index the resulting sorted, normalized sourceTerms.
tropicalSlice :: [Term3] -> Rational -> Either String SliceResult
tropicalSlice input height
    | null input = Left "At least one finite polynomial term is required."
    | length input > 32 = Left "At most 32 polynomial terms are supported."
    | otherwise = do
        curve <- tropicalCurve specialized
        pure $ SliceResult curve height sourceTerms regions
  where
    sourceTerms = map leastCoefficient $ groupBy sameExponent (sort input)
    sameExponent a b = exponent3 a == exponent3 b
    leastCoefficient = head . sortOn term3Coefficient
    exponent3 t = (term3X t, term3Y t, term3Z t)
    effective t = term3Coefficient t + fromInteger (term3Z t) * height
    sameProjection a b = projection a == projection b
    projection t = (term3X t, term3Y t)
    projectedGroups = groupBy sameProjection $ sortOn projectedKey sourceTerms
    projectedKey t = (term3X t, term3Y t, effective t, term3Z t, term3Coefficient t)
    projectedMinimum group = minimum (map effective group)
    projected =
        [ Term (term3X (head group)) (term3Y (head group)) (projectedMinimum group)
        | group <- projectedGroups ]
    specialized = projected
    tiedGroups =
        [ (group, [i | (i,t) <- zip [0..] sourceTerms, t `elem` group,
                        effective t == projectedMinimum group])
        | group <- projectedGroups
        ]
    regions = sortOn regionTerms
        [ SliceRegion ids (constraints group)
        | (group, ids) <- tiedGroups, length ids >= 2 ]
    constraints group = sort . map constraint $
        [other | other <- projectedGroups, projection (head other) /= projection (head group)]
      where
        g = head group
        gx = term3X g
        gy = term3Y g
        gc = projectedMinimum group
        constraint other =
            let k = head other
                kc = projectedMinimum other
            in SliceInequality (gx - term3X k) (gy - term3Y k) (kc - gc)
