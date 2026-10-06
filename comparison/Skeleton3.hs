-- | Exact ONE-SKELETON of a min-plus hypersurface in three variables.
-- This is the vertices and edges dual to three- and two-dimensional cells of
-- the regular Newton subdivision, NOT the entire two-dimensional root surface.
-- It is intended for comparison with the historical general TropHyp output.
-- No hull enumeration, LRS, or shared subdivision dualizer is used here.
module Skeleton3
    ( Edge3(..), Skeleton3(..), exactSkeleton3, canonicalSkeleton3
    , skeleton3Checks
    ) where

import Prelude hiding (exponent)
import Data.List (findIndex, foldl', groupBy, nub, sort, sortOn)
import Data.Maybe (mapMaybe)
import Data.Ratio (denominator, numerator)
import Geometry.TropicalSlice (Term3(..))

data Edge3
    = Segment3 [Rational] [Rational]
    | Ray3 [Rational] [Integer]
    | Line3 [Rational] [Integer]
    deriving (Eq, Ord, Show)

data Skeleton3 = Skeleton3
    { vertices3 :: [[Rational]], edges3 :: [Edge3]
    } deriving (Eq, Show)

-- | Three distinct active terms with two independent exponent differences
-- define an affine line p+t*d. Intersect their common minimum condition with
-- every other term using exact scalar inequalities. Positive-length intervals
-- give edges; isolated intersections are recovered as incident edge endpoints.
-- Triple differences of rank below two cannot define one-skeleton edges.
exactSkeleton3 :: [Term3] -> Either String Skeleton3
exactSkeleton3 input
    | null input = Left "exactSkeleton3: nonempty finite polynomial required"
    | length input > 32 = Left "exactSkeleton3: at most 32 terms are supported"
    | rank [zipWith (-) (exponent t) (exponent (head terms)) | t <- tail terms] < 3 =
        Left "exactSkeleton3: exponent support must have affine rank three"
    | otherwise = Right . canonicalSkeleton3 $ mapMaybe (tripleEdge terms) (triples terms)
  where
    terms = map (head . sortOn term3Coefficient) . groupBy sameExponent $ sort input
    sameExponent a b = exponent a == exponent b

exponent :: Term3 -> [Rational]
exponent t = map fromInteger [term3X t,term3Y t,term3Z t]

triples :: [a] -> [(a,a,a)]
triples xs = [(a,b,c) | (i,a) <- zip [0 :: Int ..] xs,
                       (j,b) <- zip [0 :: Int ..] xs, j > i,
                       c <- drop (j+1) xs]

tripleEdge :: [Term3] -> (Term3,Term3,Term3) -> Maybe Edge3
tripleEdge terms (a,b,c) = do
    let u = zipWith (-) (exponent a) (exponent b)
        v = zipWith (-) (exponent a) (exponent c)
        rawDirection = cross u v
    free <- findIndex (/= 0) rawDirection
    let direction = orient (primitive rawDirection)
        indices = filter (/= free) [0,1,2]
        j = head indices
        k = last indices
        determinant = u !! j * v !! k - u !! k * v !! j
        rhs1 = term3Coefficient b - term3Coefficient a
        rhs2 = term3Coefficient c - term3Coefficient a
        pj = (rhs1 * v !! k - u !! k * rhs2) / determinant
        pk = (u !! j * rhs2 - rhs1 * v !! j) / determinant
        start = [if i == free then 0 else if i == j then pj else pk | i <- [0,1,2]]
        clip Nothing _ = Nothing
        clip (Just (lo,hi)) term
            | slope == 0 = if rhs < 0 then Nothing else Just (lo,hi)
            | slope > 0 = consistent lo (Just (maybe bound (min bound) hi))
            | otherwise = consistent (Just (maybe bound (max bound) lo)) hi
          where
            difference = zipWith (-) (exponent a) (exponent term)
            slope = dot difference (map fromInteger direction)
            rhs = term3Coefficient term - term3Coefficient a - dot difference start
            bound = rhs / slope
    (lo,hi) <- foldl' clip (Just (Nothing,Nothing)) terms
    case (lo,hi) of
        (Just l,Just h) | l < h -> Just (Segment3 (at start direction l) (at start direction h))
        (Just _,Just _) -> Nothing
        (Just l,Nothing) -> Just (Ray3 (at start direction l) direction)
        (Nothing,Just h) -> Just (Ray3 (at start direction h) (map negate direction))
        (Nothing,Nothing) -> Just (Line3 start direction)

consistent :: Maybe Rational -> Maybe Rational -> Maybe (Maybe Rational,Maybe Rational)
consistent lo hi = case (lo,hi) of
    (Just l,Just h) | l > h -> Nothing
    _ -> Just (lo,hi)

at :: [Rational] -> [Integer] -> Rational -> [Rational]
at p d t = zipWith (\x y -> x+t*fromInteger y) p d

dot :: [Rational] -> [Rational] -> Rational
dot a b = sum (zipWith (*) a b)

cross :: [Rational] -> [Rational] -> [Rational]
cross [a,b,c] [x,y,z] = [b*z-c*y,c*x-a*z,a*y-b*x]
cross _ _ = error "Skeleton3.cross: expected three coordinates"

-- Exponent differences are integral; this helper also canonicalizes rational
-- directions supplied through the exported edge adapter.
primitive :: [Rational] -> [Integer]
primitive values = map (`div` divisor) integers
  where
    scale = foldl' lcm 1 (map denominator values)
    integers = map (numerator . (* fromInteger scale)) values
    divisor = foldl' gcd 0 (map abs integers)

orient :: [Integer] -> [Integer]
orient values = case dropWhile (==0) values of
    (first:_) | first < 0 -> map negate values
    _ -> values

-- | Canonicalize already valid nonzero three-dimensional edges. Segments are
-- unoriented; rays retain orientation; complete lines use a positive-first
-- primitive direction and zero coordinate at its first nonzero direction axis.
canonicalSkeleton3 :: [Edge3] -> Skeleton3
canonicalSkeleton3 input = Skeleton3 (sort . nub $ concatMap endpoints edges) edges
  where
    edges = sort . nub $ map canonical input
    endpoints (Segment3 a b) = [a,b]
    endpoints (Ray3 a _) = [a]
    endpoints (Line3 _ _) = []
    canonical (Segment3 a b) = if a <= b then Segment3 a b else Segment3 b a
    canonical (Ray3 p d) = Ray3 p (primitive (map fromInteger d))
    canonical (Line3 p d) =
        let direction = orient (primitive (map fromInteger d))
            axis = head [i | (i,x) <- zip [0 :: Int ..] direction, x /= 0]
            shift = negate (p !! axis) / fromInteger (direction !! axis)
        in Line3 (at p direction shift) direction

rank :: [[Rational]] -> Int
rank [] = 0
rank rows
    | null (head rows) = 0
    | otherwise = case break ((/= 0) . head) rows of
        (_,[]) -> rank (map tail rows)
        (before,pivot:after) -> 1 + rank
            [zipWith (-) (tail row) (map (* (head row / head pivot)) (tail pivot))
            | row <- before ++ after]

-- | Small exact regression fixtures usable by the comparison driver's checks.
-- These are equations with hand-known one-skeletons, independent of hull code.
skeleton3Checks :: [(String,Bool)]
skeleton3Checks =
    [ ("min(0,x,y,z) has one vertex and four rays", exactSkeleton3 simplex == Right (fan [0,0,0]))
    , ("fractionally shifted simplex", exactSkeleton3 shifted == Right (fan [1/2,2/3,3/4]))
    , ("quadratic simplex with redundant support", exactSkeleton3 quadratic == Right (fan [0,0,0]))
    , ("quadratic with a bounded skeleton edge", exactSkeleton3 splitQuadratic == Right splitExpected)
    , ("permutation invariance", exactSkeleton3 (reverse shifted) == exactSkeleton3 shifted)
    , ("duplicate exponent retains minimum", exactSkeleton3 (Term3 1 0 0 10:simplex) == exactSkeleton3 simplex)
    , ("affine exponent support rejected", case exactSkeleton3 (take 3 simplex) of Left _ -> True; Right _ -> False)
    , ("canonical full line anchor", canonicalSkeleton3 [Line3 [2,3,4] [-2,-2,0]] == canonicalSkeleton3 [Line3 [0,1,4] [1,1,0]])
    ]
  where
    simplex = [Term3 0 0 0 0,Term3 1 0 0 0,Term3 0 1 0 0,Term3 0 0 1 0]
    shifted = [Term3 0 0 0 0,Term3 1 0 0 (-1/2),Term3 0 1 0 (-2/3),Term3 0 0 1 (-3/4)]
    quadratic = [Term3 x y z 0 | x <- [0..2],y <- [0..2],z <- [0..2],x+y+z <= 2]
    splitQuadratic = [Term3 0 0 0 0,Term3 1 0 0 (-1),Term3 2 0 0 0,Term3 0 1 0 0,Term3 0 0 1 0]
    splitExpected = canonicalSkeleton3
        ([Segment3 [-1,-2,-2] [1,0,0]] ++
         [Ray3 [1,0,0] d | d <- [[1,0,0],[0,1,0],[0,0,1]]] ++
         [Ray3 [-1,-2,-2] d | d <- [[0,1,0],[0,0,1],[-1,-2,-2]]])
    fan point = canonicalSkeleton3 [Ray3 point direction | direction <- [[1,0,0],[0,1,0],[0,0,1],[-1,-1,-1]]]
