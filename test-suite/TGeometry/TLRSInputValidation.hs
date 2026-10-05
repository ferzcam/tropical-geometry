module TGeometry.TLRSInputValidation (testsLRSInputValidation) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.List (isInfixOf)
import Data.Matrix (Matrix, fromLists)
import Geometry.LRS (colFromList, lrs)
import Geometry.Facet (facetEnumeration, facetEnumeration')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)

-- Force the whole lazy result, and accept only the intended ErrorCall category.
-- Matrix indexing errors or unrelated exceptions must not make a test pass.
assertLrsError :: String -> Matrix Rational -> Matrix Rational -> [Rational] -> Assertion
assertLrsError expected matrix rhs start =
    assertErrorContaining expected (lrs matrix rhs start)

assertErrorContaining :: Show a => String -> a -> Assertion
assertErrorContaining expected result = do
    outcome <- try (evaluate (length (show result)))
        :: IO (Either ErrorCall Int)
    case outcome of
        Left err -> assertBool
            ("Expected error containing " ++ show expected ++ ", got " ++ show err)
            (expected `isInfixOf` show err)
        Right _ -> assertFailure ("Expected rejection: " ++ expected)

square :: Matrix Rational
square = fromLists [[-1,0], [0,-1], [1,0], [0,1]]

squareRhs :: Matrix Rational
squareRhs = colFromList [0,0,1,1]

rhsError :: String
rhsError = "getDictionary: right-hand side must be an m-by-1 column matching the constraint rows"

testsLRSInputValidation :: TestTree
testsLRSInputValidation = testGroup "LRS input validation"
    [ testCase "RHS row count must match the constraint matrix" $
        assertLrsError rhsError square (colFromList [0,0,1]) [0,0]
    , testCase "RHS must contain exactly one column" $
        assertLrsError rhsError square (fromLists [[0,0],[0,0],[1,1],[1,1]]) [0,0]
    , testCase "Starting vertex must have the ambient dimension" $
        assertLrsError
            "getDictionary: starting vertex dimension does not match the constraint matrix"
            square squareRhs [0,0,0]
    , testCase "A square written with the opposite inequality convention is rejected" $
        -- These describe the unit square only under Ax >= b. Under the API's
        -- Ax <= b contract, [0,0] violates the last two constraints.
        assertLrsError "getDictionary: starting vertex is infeasible for A*x <= b"
            (fromLists [[1,0],[0,1],[-1,0],[0,-1]])
            (colFromList [0,0,-1,-1]) [0,0]
    , testCase "A feasible interior point is not a starting vertex" $
        assertLrsError "independent tight constraints" square squareRhs [1/2,1/2]
    , testCase "facetEnumeration rejects the planar support of f4" $
        assertErrorContaining "facetEnumeration: lower-dimensional input requires affine reduction"
            (facetEnumeration f4Support)
    , testCase "facetEnumeration rejects the planar support of f9" $
        assertErrorContaining "facetEnumeration: lower-dimensional input requires affine reduction"
            (facetEnumeration f9Support)
    , testCase "facetEnumeration' rejects the planar support of f4" $
        assertErrorContaining "facetEnumeration': lower-dimensional input requires affine reduction"
            (facetEnumeration' f4Support)
    , testCase "facetEnumeration' rejects the planar support of f9" $
        assertErrorContaining "facetEnumeration': lower-dimensional input requires affine reduction"
            (facetEnumeration' f9Support)
    ]

-- Exact lifted supports, bypassing extremalVertices and its GLPK backend.
-- f4 is the degree-three triangle's lattice points; f9 is a square.
-- Both have affine dimension two although their ambient dimension is three.
f4Support, f9Support :: [[Integer]]
f4Support = [[3,0,0],[2,1,0],[1,2,0],[0,3,0],[2,0,0],
             [1,1,0],[0,2,0],[1,0,0],[0,1,0],[0,0,0]]
f9Support = [[2,2,0],[0,2,0],[2,0,0],[0,0,0]]
