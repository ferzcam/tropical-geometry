-- | Exact root locus of a finite min-plus polynomial in two variables.
-- This API is independent of the legacy integer plotting API.
module Geometry.TropicalCurve
    ( Point, Direction, Term(..), EdgeGeometry(..), CurveVertex(..)
    , CurveEdge(..), SubdivisionCell(..), Curve(..), tropicalCurve
    ) where

import Data.List (foldl', groupBy, nub, sort, sortOn)
import Data.Maybe (mapMaybe)

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
