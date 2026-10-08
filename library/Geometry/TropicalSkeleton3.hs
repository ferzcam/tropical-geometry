-- | Exact ONE-SKELETON of a min-plus hypersurface in three variables.
-- This is the vertices and edges dual to three- and two-dimensional cells of
-- the regular Newton subdivision, NOT the entire two-dimensional root surface.
-- It is intended for comparison with the historical general TropHyp output.
-- 'exactSkeleton3' clips exact equality lines directly; the LRS route below
-- independently enumerates lifted and projected hull facets.
module Geometry.TropicalSkeleton3
    ( Edge3(..), Skeleton3(..), exactSkeleton3, lrsTropicalSkeleton3, canonicalSkeleton3
      -- * LRS subdivision steps reused by "Geometry.TropicalGraph3"
    , lowerCells3, lrsCellFacets3
    ) where

import Prelude hiding (exponent)
import Data.List (findIndex, foldl', groupBy, nub, sort, sortOn)
import Data.Maybe (mapMaybe)
import Data.Ratio (denominator, numerator)
import qualified Geometry.LRSHull as LRS
import Geometry.TropicalSlice (Term3(..))

-- | One exact one-dimensional face in weight space. Segment endpoints are
-- rational coordinate triples; ray and line directions are primitive integer
-- triples. Rays preserve their outgoing orientation, while lines are
-- canonically anchored by 'canonicalSkeleton3'.
data Edge3
    = Segment3 [Rational] [Rational]
    | Ray3 [Rational] [Integer]
    | Line3 [Rational] [Integer]
    deriving (Eq, Ord, Show)

-- | Vertices and edges of the tropical hypersurface graph dual to the
-- three- and two-dimensional cells of the regular Newton subdivision. This
-- is the one-skeleton, not the full two-dimensional tropical surface.
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

-- | Compute the exact graph one-skeleton through the independent LRS polar
-- hull route. This returns vertices, bounded edges and unbounded rays of the
-- 3-variable tropical hypersurface, not the full two-dimensional surface.
-- Inputs have exact Rational coefficients and Integer exponents, at most 32
-- terms, and exponent support of affine rank three. Lifted support must have
-- affine rank three (one flat cell) or four. Unsupported ranks are errors.
lrsTropicalSkeleton3 :: [Term3] -> Either String Skeleton3
lrsTropicalSkeleton3 input = do
    terms <- normalize3 input
    if rank [zipWith (-) (exponent t) (exponent (head terms)) | t <- tail terms] /= 3
        then Left "lrsTropicalSkeleton3: exponent support must have affine rank three"
        else pure ()
    cells <- lowerCells3 terms
    assemble3 terms cells

normalize3 :: [Term3] -> Either String [Term3]
normalize3 input
    | null input = Left "lrsTropicalSkeleton3: nonempty finite polynomial required"
    | length input > 32 = Left "lrsTropicalSkeleton3: at most 32 terms are supported"
    | otherwise = Right . map (head . sortOn term3Coefficient)
        . groupBy same . sortOn key $ input
  where
    key t = (term3X t,term3Y t,term3Z t,term3Coefficient t)
    same a b = exponent a == exponent b

type Point3R = [Rational]
type Point3I = [Integer]
type Cell3 = [Point3I]
type Facet3 = (Cell3,Point3R)

exponentI :: Term3 -> Point3I
exponentI t = [term3X t,term3Y t,term3Z t]

-- | Lower cells of the regular subdivision through the LRS polar hull of the
-- lifted support. Terms must already be normalized (distinct, sorted
-- exponents) with affine rank three. Each cell lists the exponent points that
-- are vertices of the lifted hull on one lower facet; a flat lift gives the
-- single cell of exponent-hull vertices.
lowerCells3 :: [Term3] -> Either String [Cell3]
lowerCells3 terms = case affineRank3 lifted of
    3 -> do
        planes <- LRS.lrsHull (map (map fromInteger) exponents)
        let corners = [p | p <- exponents,
                           rank [normal | (normal,bound) <- planes,
                               dot normal (map fromInteger p) == bound] == 3]
        if length corners < 4 then Left "lrsTropicalSkeleton3: flat lift has fewer than four exponent-hull vertices"
            else Right [corners]
    4 -> do
        planes <- LRS.lrsHull lifted
        let extremes = [p | p <- lifted,
                            rank [normal | (normal,bound) <- planes,
                                dot normal p == bound] == 4]
            lower = [(normal,bound) | (normal,bound) <- planes, last normal < 0]
            cells = [[take 3 (map numerator p) | p <- extremes, dot normal p == bound]
                    | (normal,bound) <- lower]
        if null cells then Left "lrsTropicalSkeleton3: lifted support has no lower facets"
            else Right (sort . nub $ map (sort . nub) cells)
    _ -> Left "lrsTropicalSkeleton3: lifted support must have affine rank three or four"
  where
    exponents = map exponentI terms
    lifted = [map fromInteger (exponentI t) ++ [term3Coefficient t] | t <- terms]

assemble3 :: [Term3] -> [Cell3] -> Either String Skeleton3
assemble3 terms cells = do
    infos <- traverse cellInfo3 cells
    let incidences = sortOn (\(face,_,_) -> face)
            [(face,vertex,direction) | (vertex,faces) <- infos,
                                       (face,direction) <- faces]
        groups = groupBy (\(a,_,_) (b,_,_) -> a == b) incidences
    edges <- fmap concat (traverse edgeFor groups)
    pure (canonicalSkeleton3 edges)
  where
    edgeFor group = case group of
        [(face,vertex,direction)] -> Right [Ray3 vertex direction]
        [(_,a,_),(_,b,_)] | a == b -> Left "lrsTropicalSkeleton3: adjacent cells have identical dual vertices"
        [(_,a,_),(_,b,_)] -> Right [Segment3 a b]
        _ -> Left "lrsTropicalSkeleton3: a projected facet is incident to more than two cells"
    cellInfo3 cell
        | rank (differences3 cell) /= 3 =
            Left "lrsTropicalSkeleton3: a lower projected cell is not full-dimensional"
        | otherwise = do
            vertex <- tropicalVertex3 terms cell
            facets <- lrsCellFacets3 cell
            pure (vertex,[(face,primitiveDirection3 normal) | (face,normal) <- facets])

tropicalVertex3 :: [Term3] -> Cell3 -> Either String Point3R
tropicalVertex3 terms cell = do
    let rows = [map fromInteger p ++ [1] | p <- cell]
        rhs = [coefficientAt3 terms p | p <- cell]
    case solveLinear3 rows rhs of
        Just [a,b,c,_] ->
            let vertex = map negate [a,b,c]
                values = [term3Coefficient t + dot vertex (map fromInteger (exponentI t)) | t <- terms]
                cellValues = [coefficientAt3 terms p + dot vertex (map fromInteger p) | p <- cell]
            in if null cellValues || any (/= head cellValues) cellValues || head cellValues /= minimum values
               then Left "lrsTropicalSkeleton3: lower cell fails exact global-minimum validation"
               else Right vertex
        _ -> Left "lrsTropicalSkeleton3: could not solve exact affine function of a lower cell"

coefficientAt3 :: [Term3] -> Point3I -> Rational
coefficientAt3 terms point = case [term3Coefficient t | t <- terms, exponentI t == point] of
    [c] -> c
    _ -> error "coefficientAt3: normalized cell exponent is absent or duplicated"

-- | Facets of one full-dimensional projected cell through LRS: each result is
-- the sorted facet corners among the cell's extreme points and the facet's
-- outward (canonically scaled) normal.
lrsCellFacets3 :: Cell3 -> Either String [Facet3]
lrsCellFacets3 cell
    | rank (differences3 cell) /= 3 = Left "lrsTropicalSkeleton3: projected cell must have affine rank three"
    | otherwise = do
        planes <- LRS.lrsHull (map (map fromInteger) cell)
        let extreme = [p | p <- cell,
                           rank [normal | (normal,bound) <- planes,
                               dot normal (map fromInteger p) == bound] == 3]
        if length extreme < 4 then Left "lrsTropicalSkeleton3: LRS found too few projected cell vertices" else pure ()
        traverse (facetCorners extreme) planes
  where
    facetCorners extreme (normal,bound) = do
        (outward,offset) <- orientPlane3 cell (normal,bound)
        let corners = sort . nub $ [p | p <- extreme,
                                 dot outward (map fromInteger p) == offset]
        if length corners < 3 then Left "lrsTropicalSkeleton3: a cell facet has fewer than three corners"
            else Right (corners,outward)

orientPlane3 :: Cell3 -> ([Rational],Rational) -> Either String ([Rational],Rational)
orientPlane3 points (normal,bound)
    | any (>0) sides && any (<0) sides = Left "lrsTropicalSkeleton3: LRS returned a non-supporting plane"
    | all (<=0) sides = Right (normal,bound)
    | all (>=0) sides = Right (map negate normal,negate bound)
    | otherwise = Left "lrsTropicalSkeleton3: cannot orient support plane"
  where
    sides = [dot normal (map fromInteger p)-bound | p <- points]

primitiveDirection3 :: [Rational] -> [Integer]
primitiveDirection3 normal = map (`div` divisor) integers
  where
    entries = map negate normal
    scale = foldl' lcm 1 (map denominator entries)
    integers = map (numerator . (* fromInteger scale)) entries
    divisor = foldl' gcd 0 (map abs integers)

affineRank3 :: [[Rational]] -> Int
affineRank3 [] = 0
affineRank3 (origin:points) = rank (map (zipWith (-) origin) points)

differences3 :: [[Integer]] -> [[Rational]]
differences3 [] = []
differences3 (origin:points) = [zipWith (\x y -> fromInteger (y-x)) origin point | point <- points]

solveLinear3 :: [[Rational]] -> [Rational] -> Maybe [Rational]
solveLinear3 rows rhs
    | null rows || length rows /= length rhs || any ((/=n) . length) rows = Nothing
    | otherwise = eliminate (zipWith (++) rows (map (:[]) rhs)) 0 0
  where
    n = length (head rows)
    eliminate matrix row col
        | col == n = if any ((/=0) . last) (drop row matrix) then Nothing
                     else Just (map last (take n matrix))
        | row == length matrix = Nothing
        | otherwise = case [i | (i,r) <- zip [row..] (drop row matrix),r !! col /= 0] of
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
