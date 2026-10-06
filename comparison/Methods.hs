{-# LANGUAGE DataKinds #-}
-- | Comparable output adapters for the tailored hull, exact tropical-curve,
-- and independent polar/LRS routes used by the workstation benchmarks.
--
-- These adapters deliberately reject inputs outside each method's contract;
-- they never silently substitute another algorithm.
module Methods
    ( CanonicalCurve(..), LegacySegment, HullFacet
    , exactCurve, tailoredCurve, lrsCurve
    , tailoredHull2, tailoredHull3, lrsHull
    , originalLegacyCurve, legacyDrawnEdges
    ) where

import Data.List (foldl', groupBy, nub, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Geometry.ConvexHull2 (convexHull2)
import Geometry.ConvexHull3 (ConvexHull(..), Point3D, convexHull3, fromFacet)
import Geometry.LRSHull (HullFacet)
import qualified Geometry.LRSHull as LRSHull
import Geometry.Polytope (projectionToR2)
import Geometry.TropicalCurve
import Polynomial.Hypersurface (hypersurface)
import Polynomial.Monomial (Lex, toMonomial)
import Polynomial.Prelude (Polynomial(..))
import Arithmetic.Numbers (Tropical(..))

-- | Vertices; weighted edges; subdivision cells as sets of exponent vectors.
newtype CanonicalCurve = CanonicalCurve
    { unCanonicalCurve :: ([Point], [(EdgeGeometry, Integer)], [[(Integer,Integer)]])
    } deriving (Eq, Show)

type LegacySegment = ((Int,Int),(Int,Int))

-- The min-plus coefficient normalization is shared by every route: duplicate
-- exponent vectors retain their smallest coefficient before any geometry.
normalizeTerms :: [Term] -> [Term]
normalizeTerms = map (head . sortOn termCoefficient)
    . groupBy sameExponent . sortOn key
  where
    key t = (termX t, termY t, termCoefficient t)
    sameExponent a b = termX a == termX b && termY a == termY b

exactCurve :: [Term] -> Either String CanonicalCurve
exactCurve terms = do
    curve <- tropicalCurve terms
    pure (canonicalFromCurve curve)

canonicalFromCurve :: Curve -> CanonicalCurve
canonicalFromCurve curve = CanonicalCurve
    ( vertices
    , edges
    , cells
    )
  where
    vertices = sort . nub $ map vertexPoint (curveVertices curve)
    edges = sort . nub $ map canonicalWeightedEdge (curveEdges curve)
    canonicalWeightedEdge edge = (canonicalGeometry (edgeGeometry edge), edgeWeight edge)
    cells = sort . nub $ map cellExponents (curveCells curve)
    cellExponents cell = sort . nub $
        [ (termX term, termY term)
        | index <- cellBoundary cell
        , let term = curveTerms curve !! index
        ]

canonicalGeometry :: EdgeGeometry -> EdgeGeometry
canonicalGeometry (Segment a b) = if a <= b then Segment a b else Segment b a
canonicalGeometry geometry = geometry

-- | Historical tailored route: convexHull3 on integer (x,y,coefficient)
-- points, then lower-face projection. The dual curve is reconstructed from
-- those cells using exact Rational arithmetic common to this adapter and LRS.
tailoredCurve :: [Term] -> Either String CanonicalCurve
tailoredCurve input = do
    terms <- requirePlanarSupport input
    lifted <- traverse integerLift terms
    hull <- maybe (Left "tailoredCurve: empty lifted hull") Right (convexHull3 lifted)
    let cells = map (map (\(x,y) -> (toInteger x,toInteger y))) (projectionToR2 hull)
    dualizeCells terms cells

-- | Independent LRS route: polar H-to-V enumeration of the lifted point set,
-- followed by its lower supporting facets and the same exact cell dualizer.
lrsCurve :: [Term] -> Either String CanonicalCurve
lrsCurve input = do
    terms <- requirePlanarSupport input
    let points = map liftedRational terms
    case affineRank points of
        2 -> do
            -- A flat lift induces one cell: recover its actual corners from
            -- the independent LRS hull of the exponent support. Rank two
            -- active outward normals identify vertices without a tailored
            -- hull or fallback to the exact solver.
            let exponentPoints =
                    [[fromInteger (termX t),fromInteger (termY t)] | t <- terms]
            exponentFacets <- LRSHull.lrsHull exponentPoints
            let corners =
                    [ (termX t,termY t)
                    | (t,p) <- zip terms exponentPoints
                    , rank [normal | (normal,bound) <- exponentFacets,
                                     dot normal p == bound] == 2
                    ]
            if length corners < 3
                then Left "lrsCurve: flat lift has fewer than three exponent-hull vertices."
                else dualizeCells terms [corners]
        3 -> do
            facets <- LRSHull.lrsHull points
            let lower = [(normal,bound) | (normal,bound) <- facets, normal !! 2 < 0]
                cells =
                    [ [(termX t,termY t) | (t,p) <- zip terms points, dot normal p == bound]
                    | (normal,bound) <- lower
                    ]
            dualizeCells terms cells
        _ -> Left "lrsCurve: lifted support must have affine dimension two or three."

requirePlanarSupport :: [Term] -> Either String [Term]
requirePlanarSupport input
    | null input = Left "A nonempty polynomial is required."
    | length support < 3 = Left "Affine exponent support is unsupported by subdivision adapters."
    | rank (differences (map exponentPoint support)) < 2 =
        Left "Affine exponent support is unsupported by subdivision adapters."
    | otherwise = Right support
  where
    support = normalizeTerms input
    exponentPoint t = [termX t, termY t]

integerLift :: Term -> Either String Point3D
integerLift term = do
    x <- boundedInt "x exponent" (termX term)
    y <- boundedInt "y exponent" (termY term)
    coefficient <- if denominator (termCoefficient term) == 1
        then boundedInt "coefficient" (numerator (termCoefficient term))
        else Left "The tailored hull route requires integral coefficients."
    pure (x,y,coefficient)

affineRank :: [[Rational]] -> Int
affineRank [] = 0
affineRank (origin:points) = rank (map (zipWith (-) origin) points)

boundedInt :: String -> Integer -> Either String Int
boundedInt label value
    | value < toInteger (minBound :: Int) || value > toInteger (maxBound :: Int) =
        Left (label ++ " is outside the legacy Int range.")
    | otherwise = Right (fromInteger value)

liftedRational :: Term -> [Rational]
liftedRational term =
    [fromInteger (termX term), fromInteger (termY term), termCoefficient term]

-- | Reconstruct the original bivariate hull API's outward normalized facet
-- inequalities from its 2D boundary vertices.
tailoredHull2 :: [[Rational]] -> Either String [HullFacet]
tailoredHull2 input = do
    points <- integerPointCloud 2 input
    if rank (differences points) /= 2
        then Left "tailoredHull2: points must have full affine dimension 2"
        else pure ()
    let xy = [(fromInteger (p !! 0), fromInteger (p !! 1)) | p <- points]
        boundary = convexBoundary [(toInteger x,toInteger y) | (x,y) <- convexHull2 xy]
    planes <- traverse (supportingLine points) (cyclePairs boundary)
    pure (sort . nub $ planes)

-- | Reconstruct outward facet inequalities from the original incremental
-- 3D convex hull. Only full-dimensional clouds have a unique ambient normal.
tailoredHull3 :: [[Rational]] -> Either String [HullFacet]
tailoredHull3 input = do
    points <- integerPointCloud 3 input
    if rank (differences points) /= 3
        then Left "tailoredHull3: points must have full affine dimension 3"
        else pure ()
    hull <- maybe (Left "tailoredHull3: empty hull") Right
        (convexHull3 (map toPoint3D points))
    let planes = map (supportingPlane3 points . map point3ToList . fromFacet)
            (facets hull)
    if any isLeftPlane planes
        then Left "tailoredHull3: a facet did not define a supporting plane"
        else pure (sort . nub $ [plane | Right plane <- planes])
  where
    isLeftPlane (Left _) = True
    isLeftPlane _ = False

-- | LRS point-cloud-to-facet adapter, using the independent exact polar route.
lrsHull :: [[Rational]] -> Either String [HullFacet]
lrsHull = LRSHull.lrsHull

integerPointCloud :: Int -> [[Rational]] -> Either String [[Integer]]
integerPointCloud dimension input
    | null input = Left "A nonempty point set is required."
    | any ((/= dimension) . length) input = Left "Point dimensions do not match the selected hull method."
    | otherwise = traverse (traverse integralCoordinate) (sort . nub $ input)
  where
    integralCoordinate value
        | denominator value /= 1 = Left "The tailored hull methods require integral coordinates."
        | otherwise = do
            _ <- boundedInt "coordinate" (numerator value)
            pure (numerator value)

differences :: [[Integer]] -> [[Rational]]
differences [] = []
differences (origin:points) = map (zipWith (\x y -> fromInteger (x-y)) origin) points

rank :: [[Rational]] -> Int
rank [] = 0
rank rows
    | null (head rows) = 0
    | otherwise = case break ((/= 0) . head) rows of
        (_,[]) -> rank (map tail rows)
        (before,pivot:after) -> 1 + rank
            [ zipWith (-) (tail row) (map (* (head row / head pivot)) (tail pivot))
            | row <- before ++ after
            ]

dot :: [Rational] -> [Rational] -> Rational
dot a b = sum (zipWith (*) a b)

toPoint3D :: [Integer] -> Point3D
toPoint3D [x,y,z] = (fromInteger x,fromInteger y,fromInteger z)
toPoint3D _ = error "toPoint3D: expected three coordinates"

point3ToList :: Point3D -> [Integer]
point3ToList (x,y,z) = [toInteger x,toInteger y,toInteger z]

supportingLine :: [[Integer]] -> ((Integer,Integer),(Integer,Integer)) -> Either String HullFacet
supportingLine points ((x1,y1),(x2,y2))
    | all (<= 0) side = Right (normalizePlane (map fromInteger candidates,bound0))
    | all (>= 0) side = Right (normalizePlane (map (fromInteger . negate) candidates,negate bound0))
    | otherwise = Left "tailoredHull2: boundary edge does not support the input point set"
  where
    dx = x2-x1
    dy = y2-y1
    candidates = [dy, negate dx]
    bound0 = fromInteger (head candidates*x1 + last candidates*y1)
    side = [fromInteger (head candidates*(p !! 0) + last candidates*(p !! 1)) - bound0
           | p <- points]

supportingPlane3 :: [[Integer]] -> [[Integer]] -> Either String HullFacet
supportingPlane3 points face
    | length face < 3 = Left "Facet has fewer than three vertices."
    | normal0 == Nothing = Left "Facet vertices are collinear."
    | otherwise =
        let normal = fromMaybe [0,0,0] normal0
            bound0 = dot normal (map fromInteger (head face))
            sides = [dot normal (map fromInteger point) - bound0 | point <- points]
            (outward,bound)
                | all (<= 0) sides = (normal,bound0)
                | all (>= 0) sides = (map negate normal,negate bound0)
                | otherwise = (normal,bound0)
        in if any (> 0) [dot outward (map fromInteger point)-bound | point <- points]
            then Left "Facet plane has points on both sides."
            else Right (normalizePlane (outward,bound))
  where
    normal0 = findFaceNormal face

findFaceNormal :: [[Integer]] -> Maybe [Rational]
findFaceNormal points = case
    [normal | (i,a) <- zip [0::Int ..] points,
              (j,b) <- zip [0::Int ..] points, j > i,
              (k,c) <- zip [0::Int ..] points, k > j,
              let origin = map fromInteger a,
              let u = zipWith (-) (map fromInteger b) origin,
              let v = zipWith (-) (map fromInteger c) origin,
              let normal = cross3 u v,
              normal /= [0,0,0]] of
    (normal:_) -> Just normal
    [] -> Nothing

cross3 :: [Rational] -> [Rational] -> [Rational]
cross3 [a,b,c] [x,y,z] = [b*z-c*y,c*x-a*z,a*y-b*x]
cross3 _ _ = [0,0,0]

normalizePlane :: HullFacet -> HullFacet
normalizePlane (normal,bound) = (init normalized,last normalized)
  where
    entries = normal ++ [bound]
    scale = foldl' lcm 1 (map denominator entries)
    integers = map (numerator . (* fromInteger scale)) entries
    divisor = foldl' gcd 0 (map abs integers)
    normalized = map (fromInteger . (`div` divisor)) integers

cyclePairs :: [a] -> [(a,a)]
cyclePairs [] = []
cyclePairs points = zip points (tail points ++ take 1 points)

convexBoundary :: [(Integer,Integer)] -> [(Integer,Integer)]
convexBoundary input
    | length points <= 2 = points
    | otherwise = init lower ++ init upper
  where
    points = sort . nub $ input
    half = reverse . foldl' push []
    lower = half points
    upper = half (reverse points)
    push (b:a:rest) point | cross a b point <= 0 = push (a:rest) point
    push acc point = point:acc
    cross (ax,ay) (bx,by) (cx,cy) =
        (bx-ax)*(cy-ay)-(by-ay)*(cx-ax)

-- Exact regular subdivision dualization shared by tailored and LRS routes.
dualizeCells :: [Term] -> [[(Integer,Integer)]] -> Either String CanonicalCurve
dualizeCells terms rawCells
    | null cells = Left "No two-dimensional lower subdivision cells were found."
    | any ((< 3) . length) cells = Left "Affine exponent support is unsupported by subdivision adapters."
    | otherwise = do
        duals <- traverse dualVertex cells
        let cellsWithDual = zip cells duals
            vertices = sort . nub $ duals
            incidence = Map.fromListWith (++)
                [ (canonicalPair edge, [(polygon,point)])
                | (polygon,point) <- cellsWithDual
                , edge <- cyclePairs polygon
                ]
        edges <- traverse makeDualEdge (Map.toList incidence)
        pure $ CanonicalCurve (vertices, sort . nub $ concat edges, sort (map sort cells))
  where
    cells = sort . nub $ map convexBoundary rawCells
    termMap = Map.fromList [((termX t,termY t),termCoefficient t) | t <- terms]
    coefficient exponent = fromMaybe (error "dualizeCells: lower face term is missing")
        (Map.lookup exponent termMap)
    dualVertex polygon = case noncollinearTriple polygon of
        Nothing -> Left "Subdivision cell has affine dimension below two."
        Just (p,q,r) -> do
            point <- solveDual p q r
            let active = map (\e -> coefficient e + fromInteger (fst e)*fst point + fromInteger (snd e)*snd point) polygon
                allValues = [termCoefficient t + fromInteger (termX t)*fst point + fromInteger (termY t)*snd point | t <- terms]
            if null active || any (/= head active) active || head active /= minimum allValues
                then Left "Subdivision cell is not an exact projected lower face."
                else Right point
    solveDual (x1,y1) (x2,y2) (x3,y3) =
        let a = x2-x1; b = y2-y1; c = x3-x1; d = y3-y1
            rhs1 = coefficient (x1,y1) - coefficient (x2,y2)
            rhs2 = coefficient (x1,y1) - coefficient (x3,y3)
            det = fromInteger (a*d-b*c)
        in if det == 0 then Left "Subdivision cell has collinear support."
           else Right ((rhs1*fromInteger d-fromInteger b*rhs2)/det,
                       (fromInteger a*rhs2-rhs1*fromInteger c)/det)
    makeDualEdge (edge@(a,b), occurrences) = case occurrences of
        [(polygon,point)] -> do
            direction <- inwardDirection polygon edge
            let weight = latticeLength a b
            Right [(Ray point direction,weight)]
        [(_,p),(_,q)]
            | p == q -> Right []
            | otherwise -> Right [(canonicalGeometry (Segment p q), latticeLength a b)]
        _ -> Left "Subdivision edge has nonmanifold cell incidence."
    inwardDirection polygon (a,b) = case [p | p <- polygon, p /= a, p /= b] of
        [] -> Left "Boundary edge has no interior point for its cell."
        (third:_) ->
            let dx = fst b - fst a
                dy = snd b - snd a
                -- Rotate once, then choose the sign that points into the cell.
                right = (dy, negate dx)
                left = (negate dy, dx)
                side = cross2 a b third
                signed = if side > 0 then left else right
                (rx,ry) = signed
                divisor = gcd (abs rx) (abs ry)
            in if divisor == 0 then Left "Boundary edge has zero length."
               else Right (rx `div` divisor, ry `div` divisor)
    canonicalPair (a,b) = if a <= b then (a,b) else (b,a)

noncollinearTriple :: [(Integer,Integer)] -> Maybe ((Integer,Integer),(Integer,Integer),(Integer,Integer))
noncollinearTriple points =
    case [(a,b,c) | (i,a) <- zip [0::Int ..] points,
                    (j,b) <- zip [0::Int ..] points, j > i,
                    (k,c) <- zip [0::Int ..] points, k > j,
                    cross2 a b c /= 0] of
        (triple:_) -> Just triple
        [] -> Nothing

cross2 :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer) -> Integer
cross2 (ax,ay) (bx,by) (cx,cy) = (bx-ax)*(cy-ay)-(by-ay)*(cx-ax)

latticeLength :: (Integer,Integer) -> (Integer,Integer) -> Integer
latticeLength (x,y) (u,v) = gcd (abs (u-x)) (abs (v-y))

-- | Call the original Polynomial.Hypersurface.hypersurface API on exactly the
-- same normalized integer polynomial. Intended for regression checks on
-- fixtures with integral dual vertices; its historical partial/error behavior
-- is preserved rather than hidden by a fallback.
originalLegacyCurve :: [Term] -> Either String [LegacySegment]
originalLegacyCurve input = do
    terms <- requirePlanarSupport input
    _ <- traverse integerLift terms
    pure (hypersurface (legacyPolynomial terms))

legacyPolynomial :: [Term] -> Polynomial (Tropical Integer) Lex 2
legacyPolynomial terms = Polynomial $ Map.fromList
    [ (toMonomial [fromInteger (termX term),fromInteger (termY term)],
       Tropical (numerator (termCoefficient term)))
    | term <- terms
    ]

-- | Drawn-segment form used by the historical API: each canonical ray is
-- represented by its integer start and start+10*primitive direction.
legacyDrawnEdges :: CanonicalCurve -> Either String [LegacySegment]
legacyDrawnEdges (CanonicalCurve (_,edges,_)) = sort . nub <$> traverse draw edges
  where
    draw (Segment a b,_) = (,) <$> intPoint a <*> intPoint b
    draw (Ray p (dx,dy),_) = do
        start <- intPoint p
        end <- pointToInt (fst p + 10*fromInteger dx,snd p + 10*fromInteger dy)
        pure (start,end)
    draw (Line _ _,_) = Left "Legacy hypersurface output cannot represent full lines."
    intPoint = pointToInt
    pointToInt (x,y) = (,) <$> intCoordinate x <*> intCoordinate y
    intCoordinate value
        | denominator value /= 1 = Left "Legacy hypersurface output requires integral dual vertices."
        | otherwise = boundedInt "drawn point coordinate" (numerator value)
