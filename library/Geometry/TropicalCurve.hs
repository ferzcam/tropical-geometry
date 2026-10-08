-- | Exact root locus of a finite min-plus polynomial in two variables.
-- This API is independent of the legacy integer plotting API.
module Geometry.TropicalCurve
    ( Point, Direction, Term(..), EdgeGeometry(..), CurveVertex(..)
    , CurveEdge(..), SubdivisionCell(..), Curve(..), tropicalCurve
    , lrsTropicalCurve, normalizeTerms, curveFromLowerCells
    ) where

import Data.List (foldl', groupBy, nub, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Geometry.LRSHull as LRS

type Point = (Rational, Rational)
type Direction = (Integer, Integer)

data Term = Term
    { termX :: Integer, termY :: Integer, termCoefficient :: Rational
    } deriving (Eq, Ord, Show)

data EdgeGeometry = Segment Point Point | Ray Point Direction | Line Point Direction
    deriving (Eq, Ord, Show)

data CurveVertex = CurveVertex
    { vertexId :: Int, vertexPoint :: Point, vertexTerms :: [Int]
    } deriving (Eq, Show)

data CurveEdge = CurveEdge
    { edgeId :: Int, edgeGeometry :: EdgeGeometry, edgeTerms :: [Int]
    , edgeWeight :: Integer, edgeDual :: (Int, Int)
    } deriving (Eq, Show)

data SubdivisionCell = SubdivisionCell
    { cellId :: Int, cellVertexId :: Int, cellTerms :: [Int]
    , cellBoundary :: [Int]
    } deriving (Eq, Show)

data Curve = Curve
    { curveTerms :: [Term], curveVertices :: [CurveVertex]
    , curveEdges :: [CurveEdge], curveCells :: [SubdivisionCell]
    } deriving (Eq, Show)

-- | Input and all returned geometry use exact arithmetic. Duplicate exponent
-- vectors retain only their smallest coefficient. IDs index deterministic
-- sorted output, and are stable when input terms are reordered (not when the
-- combinatorial type changes). Term IDs refer to 'curveTerms'.
--
-- For each pair i,j, parameterize their equality line p+t*d. Requiring
-- c_i+a_i.(p+t*d) <= c_k+a_k.(p+t*d) gives the exact scalar inequality
-- (a_i-a_k).d * t <= c_k-c_i-(a_i-a_k).p. Intersecting these half-lines
-- yields a segment, ray, full line, point, or empty set. Positive-dimensional
-- pieces are the root locus; isolated pair intersections are covered by their
-- incident edges. This also handles affine one-dimensional Newton supports.
tropicalCurve :: [Term] -> Either String Curve
tropicalCurve input
    | null input = Left "At least one finite polynomial term is required."
    | length input > 64 = Left "At most 64 polynomial terms are supported."
    | otherwise = Right $ Curve terms vertices edges cells
  where
    terms = map (minimumByCoefficient) $ groupBy sameExponent $ sort input
    sameExponent a b = termExponent a == termExponent b
    minimumByCoefficient = head . sortOn termCoefficient
    pairs = [(a,b) | (i,a) <- zip [0 :: Int ..] terms, b <- drop (i+1) terms]
    geometries = sort . nub $ mapMaybe (uncurry (pairEdge terms)) pairs
    points = sort . nub $ concatMap endpoints geometries
    vertices = [CurveVertex i p (active terms p) | (i,p) <- zip [0..] points]
    edges = [CurveEdge i g ids (weight terms ids) (head ids,last ids)
            | (i,g) <- zip [0..] geometries, let ids = active terms (interior g)]
    cells = [SubdivisionCell i i ids (boundary terms ids)
            | CurveVertex i _ ids <- vertices]

-- | Compute the same exact 'Curve' record through independent LRS lower-hull
-- enumeration. Duplicate exponents retain their least coefficient, and term
-- IDs index the normalized, exponent-sorted 'curveTerms'. The exponent support
-- must have affine dimension two. Its lifted coefficient support must have
-- affine dimension two (one flat subdivision cell) or three (a regular
-- subdivision). Inputs contain at most 64 terms. Unsupported ranks are errors.
lrsTropicalCurve :: [Term] -> Either String Curve
lrsTropicalCurve input
    | null input = Left "At least one finite polynomial term is required."
    | length input > 64 = Left "At most 64 polynomial terms are supported."
    | rank (affineDifferences (map exponentRational terms)) /= 2 =
        Left "lrsTropicalCurve: exponent support must have affine dimension two."
    | otherwise = do
        cells <- lowerCells terms
        assembleSubdivision "lrsTropicalCurve" terms cells
  where
    terms = map (head . sortOn termCoefficient)
        . groupBy sameExponent . sort $ input
    sameExponent a b = termExponent a == termExponent b

-- | Min-plus normalization shared by every route: duplicate exponent vectors
-- retain only their smallest coefficient, and the result is exponent-sorted so
-- term IDs are stable under input reordering.
normalizeTerms :: [Term] -> [Term]
normalizeTerms = map (head . sortOn termCoefficient) . groupBy sameExponent . sort
  where
    sameExponent a b = termExponent a == termExponent b

-- | Exact dualization of lower subdivision cells into the full 'Curve' record.
-- Each cell lists the exponent points of one lower face of the lifted support;
-- the terms must already be normalized with 'normalizeTerms'. Dual vertices,
-- incident terms, boundaries, weights, rays, and IDs are recomputed exactly and
-- validated against the global minimum, so a hull route only supplies the
-- subdivision. The label prefixes error messages with the calling route.
curveFromLowerCells :: String -> [Term] -> [[(Integer,Integer)]] -> Either String Curve
curveFromLowerCells = assembleSubdivision

lowerCells :: [Term] -> Either String [[(Integer,Integer)]]
lowerCells terms = case affineRank lifted of
    2 -> do
        facets <- LRS.lrsHull exponentPoints
        let corners = [exponentPoint t
                      | (t,p) <- zip terms exponentPoints
                      , rank [normal | (normal,bound) <- facets, dot normal p == bound] == 2]
        if length (nub corners) < 3
            then Left "lrsTropicalCurve: flat lift has fewer than three exponent-hull vertices."
            else Right [map exponentPoint terms]
    3 -> do
        facets <- LRS.lrsHull lifted
        let cells = [ [exponentPoint t | (t,p) <- zip terms lifted, dot normal p == bound]
                    | (normal,bound) <- facets, last normal < 0 ]
        if null cells
            then Left "lrsTropicalCurve: lifted support has no lower facets."
            else Right cells
    _ -> Left "lrsTropicalCurve: lifted support must have affine dimension two or three."
  where
    exponentPoints = [map fromInteger [termX t,termY t] | t <- terms]
    lifted = [map fromInteger [termX t,termY t] ++ [termCoefficient t] | t <- terms]

assembleSubdivision :: String -> [Term] -> [[(Integer,Integer)]] -> Either String Curve
assembleSubdivision label terms rawCells = do
    duals <- traverse cellDual rawCells
    let points = sort . nub $ map fst duals
        vertices = [CurveVertex i p (active terms p) | (i,p) <- zip [0..] points]
        pointIds = Map.fromList [(p,i) | (i,p) <- zip [0..] points]
        orderedDuals = sortOn fst (nub duals)
        cells = [ let ids = active terms p
                  in SubdivisionCell i (pointIds Map.! p) ids (boundary terms ids)
                | (i,(p,_)) <- zip [0..] orderedDuals ]
        incidence = Map.fromListWith (++)
            [ (canonicalPair a b, [(polygon,p)])
            | (p,polygon) <- orderedDuals
            , (a,b) <- cyclePairs polygon
            ]
    geometries <- fmap concat . traverse makeGeometry $ Map.toList incidence
    let unique = sort . nub $ geometries
    edges <- traverse (makeCurveEdge label terms) (zip [0..] unique)
    pure (Curve terms vertices edges cells)
  where
    failure message = Left (label ++ ": " ++ message)
    cellDual cell = do
        let ids = [i | (i,t) <- zip [0..] terms, exponentPoint t `elem` cell]
        if length ids < 3 then failure "lower cell has fewer than three support terms." else pure ()
        let triples = [(terms !! i,terms !! j,terms !! k)
                      | (ii,i) <- zip [0 :: Int ..] ids
                      , (jj,j) <- zip [0 :: Int ..] ids, jj > ii
                      , k <- drop (jj+1) ids
                      , noncollinear (terms !! i) (terms !! j) (terms !! k)]
        case triples of
            [] -> failure "lower cell has collinear support."
            ((a,b,c):_) -> do
                p <- solveDual label a b c
                let activeIds = active terms p
                    polygonIds = boundary terms activeIds
                    polygon = [(termX (terms !! i),termY (terms !! i)) | i <- polygonIds]
                    values = [value t p | t <- terms]
                    minimumValue = minimum values
                    faceValues = [value t p | t <- terms, exponentPoint t `elem` cell]
                if any (/= minimumValue) faceValues
                    then failure "a reported lower cell is not globally minimal."
                    else if length polygon < 3
                    then failure "lower face has affine dimension below two."
                    else Right (p,polygon)
    makeGeometry (edge@(a,b),occurrences) = case occurrences of
        [(polygon,p)] -> do
            d <- inwardDirection polygon edge
            Right [Ray p d]
        [(_,p),(_,q)] | p == q -> Right []
        [(_,p),(_,q)] -> Right [if p <= q then Segment p q else Segment q p]
        _ -> failure "subdivision edge has nonmanifold cell incidence."
    inwardDirection polygon (a,b) = case [p | p <- polygon,p /= a,p /= b] of
        [] -> failure "boundary edge has no interior point."
        (third:_) ->
            let dx = fst b-fst a
                dy = snd b-snd a
                side = cross2 a b third
                (rx,ry) = if side > 0 then (negate dy,dx) else (dy,negate dx)
                divisor = gcd (abs rx) (abs ry)
            in if divisor == 0 then failure "boundary edge has zero length."
               else Right (rx `div` divisor,ry `div` divisor)

makeCurveEdge :: String -> [Term] -> (Int,EdgeGeometry) -> Either String CurveEdge
makeCurveEdge label terms (identifier,geometry) = do
    let ids = active terms (interior geometry)
    if length ids < 2 then Left (label ++ ": an edge has fewer than two active terms.") else pure ()
    pure (CurveEdge identifier geometry ids (weight terms ids) (head ids,last ids))

solveDual :: String -> Term -> Term -> Term -> Either String Point
solveDual label first second third
    | determinant == 0 = Left (label ++ ": selected cell terms are collinear.")
    | otherwise = Right (x,y)
  where
    x1 = termX first; y1 = termY first
    a = termX second-x1; b = termY second-y1
    c = termX third-x1; d = termY third-y1
    rhs1 = termCoefficient first-termCoefficient second
    rhs2 = termCoefficient first-termCoefficient third
    determinant = fromInteger (a*d-b*c)
    x = (rhs1*fromInteger d-fromInteger b*rhs2)/determinant
    y = (fromInteger a*rhs2-rhs1*fromInteger c)/determinant

noncollinear :: Term -> Term -> Term -> Bool
noncollinear a b c = (termX b-termX a)*(termY c-termY a)
                   /= (termY b-termY a)*(termX c-termX a)

exponentPoint :: Term -> (Integer,Integer)
exponentPoint t = (termX t,termY t)

exponentRational :: Term -> [Rational]
exponentRational t = [fromInteger (termX t),fromInteger (termY t)]

affineRank :: [[Rational]] -> Int
affineRank [] = 0
affineRank (origin:points) = rank (map (zipWith (-) origin) points)

affineDifferences :: [[Rational]] -> [[Rational]]
affineDifferences [] = []
affineDifferences (origin:points) = map (zipWith (-) origin) points

rank :: [[Rational]] -> Int
rank [] = 0
rank rows
    | null (head rows) = 0
    | otherwise = case break ((/= 0) . head) rows of
        (_,[]) -> rank (map tail rows)
        (before,pivot:after) -> 1 + rank
            [zipWith (-) (tail row) (map (* (head row/head pivot)) (tail pivot))
            | row <- before ++ after]

dot :: [Rational] -> [Rational] -> Rational
dot a b = sum (zipWith (*) a b)

cyclePairs :: [a] -> [(a,a)]
cyclePairs [] = []
cyclePairs values = zip values (tail values ++ take 1 values)

canonicalPair :: Ord a => a -> a -> (a,a)
canonicalPair a b = if a <= b then (a,b) else (b,a)

cross2 :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer) -> Integer
cross2 (ax,ay) (bx,by) (cx,cy) = (bx-ax)*(cy-ay)-(by-ay)*(cx-ax)

termExponent :: Term -> (Integer, Integer)
termExponent t = (termX t, termY t)

value :: Term -> Point -> Rational
value (Term a b c) (x,y) = c + fromInteger a*x + fromInteger b*y

active :: [Term] -> Point -> [Int]
active terms p = [i | (i,v) <- zip [0..] values, v == minimum values]
  where values = map (`value` p) terms

at :: Point -> Direction -> Rational -> Point
at (x,y) (a,b) t = (x+fromInteger a*t, y+fromInteger b*t)

pairEdge :: [Term] -> Term -> Term -> Maybe EdgeGeometry
pairEdge terms i j = do
    (lo,hi) <- foldl' clip (Just (Nothing,Nothing)) terms
    case (lo,hi) of
        (Just l, Just h) | l < h -> Just (Segment (at p d l) (at p d h))
        (Just _, Just _) -> Nothing
        (Just l, Nothing) -> Just (Ray (at p d l) d)
        (Nothing, Just h) -> Just (Ray (at p d h) (negate dx,negate dy))
        (Nothing, Nothing) -> Just (Line p d)
  where
    a = termX i - termX j
    b = termY i - termY j
    c = termCoefficient j - termCoefficient i
    p = if b /= 0 then (0,c/fromInteger b) else (c/fromInteger a,0)
    divisor = gcd a b
    initial = (b `div` divisor, negate a `div` divisor)
    d@(dx,dy) = if initial < (0,0) then let (u,v)=initial in (-u,-v) else initial
    clip Nothing _ = Nothing
    clip (Just (lo,hi)) k
        | slope == 0 = if rhs < 0 then Nothing else Just (lo,hi)
        | slope > 0 = check lo (Just (maybe bound (min bound) hi))
        | otherwise = check (Just (maybe bound (max bound) lo)) hi
      where
        ax = termX i - termX k
        ay = termY i - termY k
        slope = fromInteger (ax*dx+ay*dy)
        rhs = termCoefficient k - termCoefficient i
              - fromInteger ax*fst p - fromInteger ay*snd p
        bound = rhs/slope
    check lo hi = case (lo,hi) of
        (Just l, Just h) | l > h -> Nothing
        _ -> Just (lo,hi)

endpoints :: EdgeGeometry -> [Point]
endpoints (Segment p q) = [p,q]
endpoints (Ray p _) = [p]
endpoints (Line _ _) = []

interior :: EdgeGeometry -> Point
interior (Segment (x,y) (u,v)) = ((x+u)/2,(y+v)/2)
interior (Ray p d) = at p d 1
interior (Line p _) = p

weight :: [Term] -> [Int] -> Integer
weight terms ids = gcd (x-u) (y-v)
  where
    exps = sort [termExponent (terms !! i) | i <- ids]
    (x,y) = head exps
    (u,v) = last exps

-- Counterclockwise convex boundary; redundant support points are retained in
-- cellTerms but excluded from the drawable boundary.
boundary :: [Term] -> [Int] -> [Int]
boundary terms ids = map snd $ init lower ++ init upper
  where
    points = sort [(termExponent (terms !! i),i) | i <- ids]
    lower = reverse $ foldl' step [] points
    upper = reverse $ foldl' step [] (reverse points)
    step (b:a:rest) p | cross a b p <= 0 = step (a:rest) p
    step acc p = p:acc
    cross ((x,y),_) ((u,v),_) ((a,b),_) = (u-x)*(b-y)-(v-y)*(a-x)
