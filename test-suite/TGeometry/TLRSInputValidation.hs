module TGeometry.TLRSInputValidation (testsLRSInputValidation) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.List (isInfixOf, sort)
import Data.Matrix (Matrix, fromLists)
import Geometry.LRS (colFromList, lrs)
import Geometry.Facet (facetEnumeration, facetEnumeration')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

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
    , testCase "An unbounded strip needs separate vertex and ray output" $
        assertLrsError
            "lrs: unbounded non-homogeneous input requires separate vertex and ray output"
            (fromLists [[-1,0],[0,-1],[0,1]])
            (colFromList [0,0,1]) [0,0]
    , testCase "A shifted quadrant needs separate vertex and ray output" $
        assertLrsError
            "lrs: unbounded non-homogeneous input requires separate vertex and ray output"
            (fromLists [[-1,0],[0,-1]])
            (colFromList [-1,-1]) [1,1]
    , testCase "facetEnumeration enumerates the planar support of f4" $
        enumerate f4Support @?= [[0,0,0],[0,3,0],[3,0,0]]
    , testCase "facetEnumeration enumerates the planar support of f9" $
        enumerate f9Support @?= sort (map (map toRational) f9Support)
    , testCase "facetEnumeration' preserves ambient facet vertices for f4" $
        checkFacets f4Support
    , testCase "facetEnumeration' preserves ambient facet vertices for f9" $
        checkFacets f9Support
    , testCase "Historical f5 lifted triangle with negative coordinates" $
        enumerate [[0,0,-2],[0,-1,2],[1,-1,2]] @?=
            [[0,-1,2],[0,0,-2],[1,-1,2]]
    , testCase "Tilted translated plane with rational affine coefficients and edge points" $
        enumerate [[2,3,5],[4,3,6],[4,7,10],[2,7,9],[2,5,7]] @?=
            [[2,3,5],[2,7,9],[4,3,6],[4,7,10]]
    , testCase "Plane whose first coordinate is constant" $
        enumerate [[7,0,0],[7,2,0],[7,0,2],[7,1,1]] @?=
            [[7,0,0],[7,0,2],[7,2,0]]
    , testCase "Translated segment discards interior and duplicate points" $
        enumerate [[3,6,6],[4,8,9],[2,4,3],[4,8,9]] @?=
            [[2,4,3],[4,8,9]]
    , testCase "Planar reordered support with duplicate corners and interior point first" $
        enumerate [[1,1,0],[2,2,0],[0,0,0],[2,0,0],[0,2,0],[2,2,0]] @?=
            [[0,0,0],[0,2,0],[2,0,0],[2,2,0]]
    , testCase "Ambient one-dimensional segment" $
        enumerate [[2],[5],[-1],[2]] @?= [[-1],[5]]
    , testCase "Origin singleton has no nonzero ray directions" $
        enumerate [[0,0,0]] @?= [[0,0,0]]
    , testCase "Full-dimensional translated tetrahedron preserves facet incidence" $
        checkFacets [[2,3,4],[3,3,4],[2,4,4],[2,3,5]]
    , testCase "Flat square enumerates from every corner with reversed constraints" $ do
        let points = [[2,3,5],[4,3,6],[4,7,10],[2,7,9]]
            hs = reverse (facetEnumeration points)
            matrix = fromLists [h | (_,h,_) <- hs]
            rhs = colFromList [b | (_,_,b) <- hs]
            expected = sort (map (map toRational) points)
        mapM_ (\p -> lrs matrix rhs (map toRational p) @?= expected) points
    , testCase "Singleton is represented by affine equalities" $
        enumerate [[2,-3,5],[2,-3,5]] @?= [[2,-3,5]]

    ]

-- Explicit lifted supports and independent expected vertices. Intrinsic facet
-- construction still uses the existing extremalVertices/GLPK backend.
-- f4 is the degree-three triangle's lattice points; f9 is a square.
-- Both have affine dimension two although their ambient dimension is three.
f4Support, f9Support :: [[Integer]]
f4Support = [[3,0,0],[2,1,0],[1,2,0],[0,3,0],[2,0,0],
             [1,1,0],[0,2,0],[1,0,0],[0,1,0],[0,0,0]]
f9Support = [[2,2,0],[0,2,0],[2,0,0],[0,0,0]]


-- The lexicographic minimum of a finite point set is always an extreme point.
enumerate :: [[Integer]] -> [[Rational]]
enumerate points =
    let hs = facetEnumeration points
    in lrs (fromLists [h | (_,h,_) <- hs])
           (colFromList [b | (_,_,b) <- hs])
           (map toRational (minimum points))

checkFacets :: [[Integer]] -> Assertion
checkFacets points = do
    let indexed = facetEnumeration points
        explicit = facetEnumeration' points
    explicit @?= [(map ((sort points !!) . subtract 1) ids,h,b) | (ids,h,b) <- indexed]
    assertBool "Every facet vertex lies on its plane and every support point satisfies every inequality"
        (all (\(vs,h,b) -> not (null vs) &&
            all (\v -> sum (zipWith (*) h (map toRational v)) == b) vs &&
            all (\v -> sum (zipWith (*) h (map toRational v)) <= b) points) explicit)
