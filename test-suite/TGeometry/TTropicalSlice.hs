module TGeometry.TTropicalSlice (testsTropicalSlice) where

import Data.Ratio ((%))
import Geometry.TropicalCurve
    ( Curve(..), CurveEdge(..), CurveVertex(..), EdgeGeometry(..), Term(..), curveEdges )
import Geometry.TropicalSlice
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

withSlice :: [Term3] -> Rational -> (SliceResult -> IO ()) -> IO ()
withSlice terms height action = case tropicalSlice terms height of
    Left err -> assertFailure err
    Right result -> action result

testsTropicalSlice :: TestTree
testsTropicalSlice = testGroup "Exact tropical hypersurface slices"
    [ testCase "min(0,x,y,z) shifts the curve below and preserves it above" $ do
        withSlice [Term3 0 0 0 0, Term3 1 0 0 0, Term3 0 1 0 0, Term3 0 0 1 0] (-1) $ \r ->
            assertEqual "translated vertex" [(-1,-1)] (map vertexPoint (curveVertices (sliceCurve r)))
        withSlice [Term3 0 0 0 0, Term3 1 0 0 0, Term3 0 1 0 0, Term3 0 0 1 0] 1 $ \r ->
            assertEqual "standard vertex" [(0,0)] (map vertexPoint (curveVertices (sliceCurve r)))
    , testCase "height zero reports the filled positive quadrant" $
        withSlice [Term3 0 0 0 0, Term3 1 0 0 0, Term3 0 1 0 0, Term3 0 0 1 0] 0 $ \r -> do
            assertEqual "quadrant H-representation"
                [SliceRegion [0,1] [SliceInequality (-1) 0 0, SliceInequality 0 (-1) 0]]
                (sliceRegions r)
    , testCase "min(0,z) is the whole plane at zero and empty away from zero" $ do
        withSlice [Term3 0 0 0 0, Term3 0 0 1 0] 0 $ \r ->
            assertEqual "whole plane has no boundary inequalities"
                [SliceRegion [0,1] []] (sliceRegions r)
        withSlice [Term3 0 0 0 0, Term3 0 0 1 0] 1 $ \r -> do
            assertEqual "no tied source terms" [] (sliceRegions r)
            assertEqual "one specialized monomial" [] (curveEdges (sliceCurve r))
    , testCase "duplicate identical 3D terms do not create a filled region" $
        withSlice [Term3 0 0 0 0, Term3 0 0 0 0, Term3 1 0 0 0] 0 $ \r -> do
            assertEqual "duplicates normalize away" 2 (length (sliceSourceTerms r))
            assertEqual "no false region" [] (sliceRegions r)
    , testCase "rational height is substituted exactly" $
        withSlice [Term3 0 0 0 0, Term3 0 0 2 0, Term3 1 0 2 0] ((1)%6) $ \r -> do
            assertEqual "retained exact slice height" (1%6) (sliceHeight r)
            assertEqual "half coefficient" ((1)%3)
                (termCoefficientAt (sliceCurve r) (1,0))
    , testCase "a cubic cycle collapses at the exact lifting threshold" $ do
        withSlice cubic 0 $ \r ->
            assertEqual "genus-one cycle rank" 1 (boundedCycleRank (sliceCurve r))
        withSlice cubic 1 $ \r ->
            assertEqual "collapsed cycle rank" 0 (boundedCycleRank (sliceCurve r))
    ]
  where
    cubic =
        [ Term3 i j (if (i,j) == (1,1) then 1 else 0)
            (fromInteger (i*i+i*j+j*j))
        | i <- [0..3], j <- [0..3-i]
        ]
    boundedCycleRank curve = edgeCount - vertexCount + 1
      where
        edgeCount = length [() | e <- curveEdges curve, Segment _ _ <- [edgeGeometry e]]
        vertexCount = length (curveVertices curve)
    termCoefficientAt curve (x,y) =
        case [termCoefficient t | t <- curveTerms curve, (termX t,termY t) == (x,y)] of
            [c] -> c
            _ -> error "expected one specialized term at the requested exponent"
