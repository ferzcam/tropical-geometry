-- | Exact three-dimensional sections of four-variable min-plus polynomials.
-- Each patch is the polyhedron where a pair of specialized terms tie for
-- the minimum. The equality is kept separate from its half-space constraints.
module Geometry.TropicalSlice4
    ( Term4(..), Slice3Patch(..), Slice4Result(..), tropicalSlice4
    ) where

import Data.List (groupBy, sort, sortOn)

data Term4 = Term4
    { term4X :: Integer, term4Y :: Integer, term4Z :: Integer, term4W :: Integer
    , term4Coefficient :: Rational
    } deriving (Eq, Ord, Show)

data Slice3Patch = Slice3Patch
    { patchTerms :: (Int, Int)
    , patchEquality :: (Integer, Integer, Integer, Rational)
    , patchInequalities :: [(Integer, Integer, Integer, Rational)]
    } deriving (Eq, Show)

data Slice4Result = Slice4Result
    { slice4Height :: Rational
    , slice4SourceTerms :: [Term4]
    , slice4Patches :: [Slice3Patch]
    } deriving (Eq, Show)

-- | Intersect the 4D tropical hypersurface with w = height. Duplicate
-- original exponents retain their least coefficient. Patch IDs refer to the
-- sorted normalized source terms returned with the result.
tropicalSlice4 :: [Term4] -> Rational -> Either String Slice4Result
tropicalSlice4 input height
    | null input = Left "At least one finite polynomial term is required."
    | length input > 32 = Left "At most 32 terms are supported."
    | otherwise = Right $ Slice4Result height sourceTerms patches
  where
    sourceTerms = map leastCoefficient $ groupBy sameExponent (sort input)
    sameExponent a b = exponent4 a == exponent4 b
    leastCoefficient = head . sortOn term4Coefficient
    exponent4 t = (term4X t, term4Y t, term4Z t, term4W t)
    effective t = term4Coefficient t + fromInteger (term4W t) * height
    slope t = (term4X t, term4Y t, term4Z t)
    indexed = zip [0..] sourceTerms
    pairs = [(i,a,j,b) | (i,a):rest <- tails indexed, (j,b) <- rest]
    tails [] = []
    tails xs@(_:rest) = xs : tails rest
    patches = sortOn patchTerms $ concatMap makePatch pairs
    makePatch (i,a,j,b)
        | not (tiesPossible a b) = []
        | otherwise =
            let eq = plane a b
                -- Keep the two tied terms out; their equality is represented
                -- by eq. Every other term must be at least as large.
                others = [constraint a k | k <- sourceTerms, k /= a, k /= b]
            in [Slice3Patch (i,j) eq others]
    tiesPossible a b = slope a /= slope b || effective a == effective b
    plane a b =
        ( term4X a - term4X b
        , term4Y a - term4Y b
        , term4Z a - term4Z b
        , effective b - effective a
        )
    constraint a k =
        ( term4X a - term4X k
        , term4Y a - term4Y k
        , term4Z a - term4Z k
        , effective k - effective a
        )
