module TGeometry.TTropicalHull2 (testsTropicalHull2) where

import Control.Monad (forM_)
import Data.Ratio ((%))
import Geometry.TropicalCurve
import Geometry.TropicalHull2 (hullTropicalCurve)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

testsTropicalHull2 :: TestTree
testsTropicalHull2 = testGroup "Tailored convex-hull tropical curve route"
    [ testCase "hull route returns the same exact curve record as the direct and LRS routes" $
        forM_ [lineTerms,squareTerms,hexagonTerms,mixedTerms,fractionalVertex,integerGrid,cubicTerms] $ \terms -> do
            assertEqual "direct route" (tropicalCurve terms) (hullTropicalCurve terms)
            assertEqual "LRS route" (lrsTropicalCurve terms) (hullTropicalCurve terms)
    , testCase "hull route keeps fractional dual vertices from integral input" $
        case hullTropicalCurve fractionalVertex of
            Left err -> assertFailure err
            Right curve -> assertEqual "vertex" [((-1)%2,0)] (map vertexPoint (curveVertices curve))
    , testCase "integer paraboloid lift gives four nonsimplicial square cells" $
        case hullTropicalCurve integerGrid of
            Left err -> assertFailure err
            Right curve -> do
                assertEqual "four squares" [4,4,4,4] (map (length . cellBoundary) (curveCells curve))
                assertEqual "four bounded edges" 4
                    (length [() | e <- curveEdges curve, Segment _ _ <- [edgeGeometry e]])
    , testCase "hull route normalizes duplicate exponents like the other routes" $
        assertEqual "least coefficient retained" (tropicalCurve lineTerms)
            (hullTropicalCurve (Term 1 0 4 : lineTerms))
    , testCase "hull route rejects fractional coefficients instead of substituting another solver" $
        case hullTropicalCurve [Term 0 0 0,Term 1 0 (1%3),Term 0 1 0] of
            Left err -> assertBool "names the integral contract" ("integral" `isInfixOf'` err)
            Right _ -> assertFailure "fractional coefficient was accepted"
    , testCase "hull route rejects empty, affine, and oversized input" $ do
        assertBool "empty" (isLeft (hullTropicalCurve []))
        assertBool "two monomials" (isLeft (hullTropicalCurve [Term 0 0 0,Term 1 0 0]))
        assertBool "collinear support" (isLeft (hullTropicalCurve [Term 0 0 0,Term 1 0 (-1),Term 2 0 0]))
        assertBool "more than 64 raw terms" (isLeft (hullTropicalCurve (replicate 65 (Term 0 0 0))))
    ]
  where
    lineTerms = [Term 0 0 0,Term 1 0 0,Term 0 1 0]
    squareTerms = [Term 0 0 0,Term 1 0 0,Term 0 1 0,Term 1 1 0]
    hexagonTerms = [Term x y 0 | (x,y) <- [(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0)]]
    mixedTerms = squareTerms ++ [Term 2 0 1]
    fractionalVertex = [Term 0 0 0,Term 2 0 1,Term 0 1 0]
    integerGrid = [Term x y (fromInteger (x*x+y*y)) | x <- [0..2],y <- [0..2]]
    cubicTerms = [Term x y (fromInteger (x*x+x*y+y*y)) | x <- [0..3],y <- [0..3],x+y <= 3]
    isLeft (Left _) = True
    isLeft _ = False
    isInfixOf' needle haystack = any (needle `prefixOf`) (tails' haystack)
    prefixOf [] _ = True
    prefixOf _ [] = False
    prefixOf (a:as) (b:bs) = a == b && prefixOf as bs
    tails' [] = [[]]
    tails' s@(_:rest) = s : tails' rest
