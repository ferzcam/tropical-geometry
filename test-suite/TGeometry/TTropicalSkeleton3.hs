module TGeometry.TTropicalSkeleton3 (testsTropicalSkeleton3) where

import Control.Monad (forM_)
import Data.List (sort)
import Data.Ratio ((%))
import Geometry.TropicalSlice (Term3(..))
import Geometry.TropicalSkeleton3
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

testsTropicalSkeleton3 :: TestTree
testsTropicalSkeleton3 = testGroup "Exact 3-variable tropical graph one-skeleton"
    [ testCase "LRS route matches the direct exact graph on flat, shifted, and subdivided lifts" $
        forM_ [simplex,shifted,quadratic,splitQuadratic,rationalSplitQuadratic] $ \terms ->
            assertEqual "vertices, bounded edges, and rays"
                (exactSkeleton3 terms) (lrsTropicalSkeleton3 terms)
    , testCase "rational split quadratic has two tetrahedral cells joined by one bounded edge" $
        case exactSkeleton3 rationalSplitQuadratic of
            Left err -> assertFailure err
            Right graph -> do
                assertEqual "two dual vertices" 2 (length (vertices3 graph))
                assertEqual "one bounded edge" 1 (length [() | Segment3 _ _ <- edges3 graph])
                assertEqual "six rays" 6 (length [() | Ray3 _ _ <- edges3 graph])
    , testCase "paraboloid lift of the 3x3x3 grid gives eight nonsimplicial cube cells" $ do
        -- Each unit cube's eight corners are co-spherical, so the lower hull is
        -- the subdivision into eight unit cubes. The dual vertex of the cube
        -- with lowest corner (i,j,k) is the negated cube center. The 2x2x2
        -- cube arrangement has 12 interior faces and 24 boundary faces.
        let expectedVertices = sort [[negate (fromInteger i+1%2) | i <- [a,b,c]]
                                    | a <- [0,1],b <- [0,1],c <- [0,1]]
        case lrsTropicalSkeleton3 cubeGrid of
            Left err -> assertFailure err
            Right graph -> do
                assertEqual "same graph as the direct route" (exactSkeleton3 cubeGrid) (Right graph)
                assertEqual "eight dual vertices" expectedVertices (vertices3 graph)
                assertEqual "twelve bounded edges" 12 (length [() | Segment3 _ _ <- edges3 graph])
                assertEqual "twenty-four rays" 24 (length [() | Ray3 _ _ <- edges3 graph])
                assertEqual "no complete lines" 0 (length [() | Line3 _ _ <- edges3 graph])
                forM_ [(p,q) | Segment3 p q <- edges3 graph] $ \(p,q) ->
                    assertEqual ("axis-parallel unit segment " ++ show (p,q)) [0,0,1]
                        (sort (map abs (zipWith (-) p q)))
    , testCase "LRS retains exact rational coefficients and normalizes duplicates" $ do
        let duplicate = Term3 1 0 0 9 : shifted
        assertEqual "same normalized graph" (exactSkeleton3 shifted)
            (lrsTropicalSkeleton3 duplicate)
    , testCase "LRS rejects unsupported affine rank and input size" $ do
        assertBool "rank-deficient exponent support"
            (isLeft (lrsTropicalSkeleton3 (take 3 simplex)))
        assertBool "more than 32 raw terms"
            (isLeft (lrsTropicalSkeleton3 (replicate 33 (head simplex))))
    ]
  where
    simplex = [Term3 0 0 0 0,Term3 1 0 0 0,Term3 0 1 0 0,Term3 0 0 1 0]
    shifted = [Term3 0 0 0 0,Term3 1 0 0 ((-1)%2),Term3 0 1 0 ((-2)%3),Term3 0 0 1 ((-3)%4)]
    quadratic = [Term3 x y z 0 | x <- [0..2],y <- [0..2],z <- [0..2],x+y+z <= 2]
    splitQuadratic = [Term3 0 0 0 0,Term3 1 0 0 (-1),Term3 2 0 0 0,
                      Term3 0 1 0 0,Term3 0 0 1 0]
    -- The x-axis midpoint lift -2/3 lies below its chord value 1/10, so the
    -- lifted support splits into the tetrahedra {0,1,y,z} and {1,2,y,z}.
    rationalSplitQuadratic = [Term3 0 0 0 0,Term3 1 0 0 ((-2)%3),Term3 2 0 0 (1%5),
                              Term3 0 1 0 (1%7),Term3 0 0 1 ((-1)%4)]
    cubeGrid = [Term3 x y z ((x*x+y*y+z*z)%2) | x <- [0..2],y <- [0..2],z <- [0..2]]
    isLeft (Left _) = True
    isLeft _ = False
