{-# LANGUAGE DataKinds #-}
module TGeometry.TTropicalCurve (testsTropicalCurve) where

import Control.Monad (forM_)
import Arithmetic.Numbers (Tropical(..))
import qualified Data.Map.Strict as Map
import Data.List (nub, sort)
import Data.Ratio ((%), numerator, denominator)
import Geometry.TropicalCurve
import Polynomial.Curve (tropicalCurveOf)
import Polynomial.Monomial (Lex, toMonomial)
import Polynomial.Prelude (Polynomial(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

lineTerms :: [Term]
lineTerms = [Term 0 0 0,Term 1 0 0,Term 0 1 0]

squareTerms :: [Term]
squareTerms = [Term 0 0 0,Term 1 0 0,Term 0 1 0,Term 1 1 0]

hexagonTerms :: [Term]
hexagonTerms = [Term x y 0 | (x,y) <- [(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0)]]

mixedTerms :: [Term]
mixedTerms = squareTerms ++ [Term 2 0 1]

withCurve :: [Term] -> (Curve -> Assertion) -> Assertion
withCurve terms action = case tropicalCurve terms of
    Left err -> assertFailure err
    Right curve -> action curve

testsTropicalCurve :: TestTree
testsTropicalCurve = testGroup "Exact tropical curve"
    [ testCase "integer polynomial adapter retains fractional curve vertices" $
        assertEqual "exact polynomial geometry"
            (tropicalCurve [Term 0 0 0,Term 2 0 1,Term 0 1 0])
            (tropicalCurveOf (polynomial [([0,0],Tropical (0 :: Integer)),([2,0],Tropical 1),([0,1],Tropical 0)]))
    , testCase "polynomial adapter skips infinite coefficients" $
        assertEqual "Inf is absent, not zero"
            (tropicalCurve [Term 0 0 0,Term 1 0 0])
            (tropicalCurveOf (polynomial [([0,0],Tropical (0 :: Integer)),([1,0],Tropical 0),([0,1],Inf)]))
    , testCase "polynomial adapter rejects empty and all-infinite polynomials" $ do
        assertBool "empty" (isLeft (tropicalCurveOf (polynomial [] :: Polynomial (Tropical Integer) Lex 2)))
        assertBool "all infinite" (isLeft (tropicalCurveOf (polynomial [([0,0],Inf)] :: Polynomial (Tropical Integer) Lex 2)))
    , testCase "polynomial adapter accepts exact rational coefficients" $
        assertEqual "rational polynomial geometry"
            (tropicalCurve [Term 0 0 0,Term 1 0 (1%3),Term 0 1 ((-2)%5)])
            (tropicalCurveOf (polynomial [([0,0],Tropical (0 :: Rational)),([1,0],Tropical (1%3)),([0,1],Tropical ((-2)%5))]))
    , testCase "min(0,x,y) has the three exact min-convention rays" $
        withCurve lineTerms $ \curve -> do
            assertEqual "vertex" [(0,0)] (map vertexPoint (curveVertices curve))
            assertEqual "rays" (sort [Ray (0,0) (0,1),Ray (0,0) (1,0),Ray (0,0) (-1,-1)])
                (map edgeGeometry (curveEdges curve))
            assertEqual "triangle dual" [3] (map (length . cellBoundary) (curveCells curve))
    , testCase "fractional vertex is retained exactly" $
        withCurve [Term 0 0 0,Term 2 0 1,Term 0 1 0] $ \curve -> do
            assertEqual "vertex" [(-1%2,0)] (map vertexPoint (curveVertices curve))
            assertEqual "primitive rays" (sort [Ray (-1%2,0) (0,1),Ray (-1%2,0) (1,0),Ray (-1%2,0) (-1,-2)])
                (map edgeGeometry (curveEdges curve))
    , testCase "square subdivision remains one polygon" $
        withCurve squareTerms $ \curve -> do
            assertEqual "one cell" [4] (map (length . cellBoundary) (curveCells curve))
            assertEqual "four rays" 4 (length (curveEdges curve))
    , testCase "hexagonal subdivision remains one polygon" $
        withCurve hexagonTerms $ \curve -> do
            assertEqual "one cell" [6] (map (length . cellBoundary) (curveCells curve))
            assertEqual "six rays" 6 (length (curveEdges curve))
    , testCase "adjacent square and triangle share one bounded edge" $
        withCurve mixedTerms $ \curve -> do
            assertEqual "vertices" [(-1,0),(0,0)] (map vertexPoint (curveVertices curve))
            assertEqual "cell sizes" [3,4] (sort (map (length . cellBoundary) (curveCells curve)))
            assertEqual "bounded edge" [Segment (-1,0) (0,0)]
                [g | e <- curveEdges curve, let g = edgeGeometry e, Segment _ _ <- [g]]
            assertEqual "five rays" 5 (length [() | e <- curveEdges curve, Ray _ _ <- [edgeGeometry e]])
    , testCase "two monomials give a complete line" $
        withCurve [Term 0 0 0,Term 1 0 0] $ \curve -> do
            assertEqual "line" [Line (0,0) (0,1)] (map edgeGeometry (curveEdges curve))
            assertEqual "no artificial vertices" [] (curveVertices curve)
            assertEqual "no artificial cells" [] (curveCells curve)
    , testCase "collinear support gives two parallel lines" $
        withCurve [Term 0 0 0,Term 1 0 (-1),Term 2 0 0] $ \curve ->
            assertEqual "parallel lines" [Line (-1,0) (0,1),Line (1,0) (0,1)]
                (map edgeGeometry (curveEdges curve))
    , testCase "single monomial has empty root locus" $
        withCurve [Term (-3) 7 (2%3)] $ \curve -> do
            assertEqual "no edges" [] (curveEdges curve)
            assertEqual "no vertices" [] (curveVertices curve)
    , testCase "duplicate exponents retain only minimum coefficient" $
        assertEqual "same polynomial" (tropicalCurve lineTerms)
            (tropicalCurve (Term 1 0 5 : Term 0 0 0 : lineTerms))
    , testCase "order of input does not affect IDs or geometry" $
        assertEqual "deterministic" (tropicalCurve mixedTerms) (tropicalCurve (reverse mixedTerms))
    , testCase "redundant cell support is excluded from its boundary" $
        withCurve ([Term 0 0 0,Term 2 0 0,Term 0 2 0,Term 2 2 0] ++ [Term 1 1 0,Term 1 0 0]) $ \curve -> do
            assertEqual "all tied support" [6] (map (length . cellTerms) (curveCells curve))
            assertEqual "square boundary" [4] (map (length . cellBoundary) (curveCells curve))
            assertEqual "lattice weights" [2,2,2,2] (map edgeWeight (curveEdges curve))
    , testCase "dominated support creates no spurious locus" $
        withCurve (Term 1 1 10 : [Term 0 0 0,Term 2 0 0,Term 0 2 0,Term 2 2 0]) $ \curve ->
            assertEqual "four genuine edges" 4 (length (curveEdges curve))
    , testCase "arbitrarily large integer exponents preserve exact directions" $
        let n = 10^(30 :: Int)
        in withCurve [Term 0 0 0,Term n 0 1,Term 0 1 0] $ \curve -> do
            assertEqual "tiny rational vertex" [((-1)%n,0)] (map vertexPoint (curveVertices curve))
            assertBool "primitive direction keeps full integer" (Ray ((-1)%n,0) (-1,-n) `elem` map edgeGeometry (curveEdges curve))
    , testCase "sampled edges attain the minimum and vertices balance" $
        forM_ [lineTerms,squareTerms,hexagonTerms,mixedTerms,
               [Term 0 0 0,Term 2 0 1,Term 0 1 0],
               [Term 0 0 0,Term 2 0 0,Term 0 2 0]] $ \terms ->
            withCurve terms checkCurve
    , testCase "empty polynomial is rejected" $
        assertBool "empty input" (isLeft (tropicalCurve []))
    , testCase "input work limit is checked before normalization" $
        assertBool "too many terms" (isLeft (tropicalCurve (replicate 65 (Term 0 0 0))))
    ]
  where
    isLeft (Left _) = True
    isLeft _ = False

polynomial :: [([Int], Tropical a)] -> Polynomial (Tropical a) Lex 2
polynomial pairs = Polynomial (Map.fromList [(toMonomial mon,c) | (mon,c) <- pairs])

checkCurve :: Curve -> Assertion
checkCurve curve = do
    let terms = curveTerms curve
        evaluate (Term a b c) (x,y) = c+fromInteger a*x+fromInteger b*y
        at (x,y) (a,b) t = (x+fromInteger a*t,y+fromInteger b*t)
        samples (Segment p q) = [p,mid p q,q]
        samples (Ray p d) = [p,at p d (1%3),at p d 100]
        samples (Line p d) = [at p d (-100),p,at p d 100]
        mid (x,y) (u,v) = ((x+u)/2,(y+v)/2)
    forM_ (curveEdges curve) $ \edge -> do
        assertBool "positive weight" (edgeWeight edge > 0)
        let (first,last') = edgeDual edge
            a = terms !! first
            b = terms !! last'
            (dx,dy) = case edgeGeometry edge of
                Segment p q -> primitive p q
                Ray _ d -> d
                Line _ d -> d
        assertBool "dual endpoints belong to incident support"
            (first `elem` edgeTerms edge && last' `elem` edgeTerms edge)
        assertEqual "dual edge is perpendicular to curve edge" 0
            ((termX a-termX b)*dx+(termY a-termY b)*dy)
        forM_ (samples (edgeGeometry edge)) $ \p -> do
            let values = map (`evaluate` p) terms
                minimumValue = minimum values
            assertBool ("minimum attained twice at " ++ show p)
                (length (filter (== minimumValue) values) >= 2)
            forM_ (edgeTerms edge) $ \i ->
                assertEqual "reported incident term is minimizing" minimumValue (values !! i)
    forM_ (curveVertices curve) $ \vertex -> do
        let p = vertexPoint vertex
            directions = [(edgeWeight edge,d) | edge <- curveEdges curve,
                           d <- outgoing p (edgeGeometry edge)]
            total = foldr (\(w,(a,b)) (x,y) -> (x+w*a,y+w*b)) (0,0) directions
        assertEqual ("weighted balancing at " ++ show p) (0,0) total
    assertEqual "no duplicate geometry" (length (curveEdges curve))
        (length (nub (map edgeGeometry (curveEdges curve))))
    forM_ (curveCells curve) $ \cell -> do
        let ids = cellBoundary cell
            points = [(termX t,termY t) | i <- ids, let t = terms !! i]
            area = sum [x*v-y*u | ((x,y),(u,v)) <- zip points (drop 1 points ++ take 1 points)]
        assertBool "drawable boundary has positive counterclockwise area" (area > 0)
        assertBool "boundary uses incident support" (all (`elem` cellTerms cell) ids)
        assertEqual "cell/vertex dual incidence" (cellTerms cell)
            (vertexTerms (curveVertices curve !! cellVertexId cell))

outgoing :: Point -> EdgeGeometry -> [Direction]
outgoing p (Ray q d) = [d | p == q]
outgoing p (Segment q r)
    | p == q = [primitive q r]
    | p == r = [primitive r q]
outgoing _ _ = []

primitive :: Point -> Point -> Direction
primitive (x,y) (u,v) = (a `div` g,b `div` g)
  where
    dx = u-x
    dy = v-y
    common = lcm (denominator dx) (denominator dy)
    a = numerator (dx*fromInteger common)
    b = numerator (dy*fromInteger common)
    g = gcd a b
