module TGeometry.TLRSDegenerate (testsLRSDegenerate) where

import Control.Monad (forM_)
import Data.List (nub, permutations, sort)
import Data.Matrix (fromLists)
import Geometry.LRS (colFromList, lrs)
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

type Point = [Rational]
type Inequality = (Point, Rational)

dot :: Point -> Point -> Rational
dot a x = sum (zipWith (*) a x)

checkVertices :: [Inequality] -> [Point] -> Point -> Assertion
checkVertices rows expected start = do
    let feasible x = all (\(a, b) -> dot a x <= b) rows
        actual = lrs (fromLists (map fst rows))
                     (colFromList (map snd rows)) start
        context = "starting vertex " ++ show start
    assertBool (context ++ ": expected fixture vertices are feasible")
        (all feasible expected)
    assertBool (context ++ ": start is feasible") (feasible start)
    assertEqual (context ++ ": exact sorted unique vertices")
        (sort (nub expected)) actual
    assertBool (context ++ ": outputs satisfy Ax <= b")
        (all feasible actual)

-- Each case gets a thirty-second limit, including all its starting vertices.
-- Wrong results and exceptions remain failures; these are not expected failures.
familyTests :: String -> ([Inequality], [Point]) -> TestTree
familyTests name (rows, vertices) = testGroup name
    [boundedCase "all starting vertices" rows vertices
    ,boundedCase "reversed rows at representative starts" (reverse rows)
        [head vertices, vertices !! (length vertices `div` 2), last vertices]]
  where
    boundedCase label orderedRows starts =
        localOption (mkTimeout 30000000) $ testCase label $
            forM_ starts (checkVertices orderedRows vertices)

-- The polar of the hypercube: all 2^d sign vectors are facet normals.
-- Its 2d vertices are the positive/negative coordinate unit vectors.
-- More than d facets meet at every vertex for d >= 4.
crossPolytope :: Int -> ([Inequality], [Point])
crossPolytope dimension =
    ([(signs, 1) | signs <- sequence (replicate dimension [-1,1])],
     [[if j == i then sign else 0 | j <- [0..dimension-1]]
     | i <- [0..dimension-1], sign <- [-1,1]])

-- Delta(2,4): binary four-vectors of weight two, projected onto their
-- first three coordinates after eliminating x4 = 2-x1-x2-x3.
-- Facets are 0 <= xi <= 1 and 1 <= x1+x2+x3 <= 2.
hypersimplex24 :: ([Inequality], [Point])
hypersimplex24 =
    ([([-1,0,0],0), ([0,-1,0],0), ([0,0,-1],0)
     ,([1,0,0],1), ([0,1,0],1), ([0,0,1],1)
     ,([-1,-1,-1],-1), ([1,1,1],2)],
     sort (nub (map (take 3) (permutations [1,1,0,0]))))

-- B3 in coordinates (a,b,c,d), the upper-left 2x2 submatrix:
--   [ a       b       1-a-b   ]
--   [ c       d       1-c-d   ]
--   [ 1-a-c   1-b-d   a+b+c+d-1 ]
-- Nonnegativity of its nine entries gives nine inequalities. The six
-- permutation matrices independently supply the complete vertex set.
birkhoff3 :: ([Inequality], [Point])
birkhoff3 =
    ([([-1,0,0,0],0), ([0,-1,0,0],0)
     ,([0,0,-1,0],0), ([0,0,0,-1],0)
     ,([1,1,0,0],1), ([0,0,1,1],1)
     ,([1,0,1,0],1), ([0,1,0,1],1)
     ,([-1,-1,-1,-1],-1)],
     sort [[entry p 0 0, entry p 0 1, entry p 1 0, entry p 1 1]
          | p <- permutations [0 :: Int,1,2]])
  where
    entry permutation row column = if permutation !! row == column then 1 else 0

testsLRSDegenerate :: TestTree
testsLRSDegenerate = testGroup "LRS degenerate bounded-polytope regressions"
    [familyTests "cross-polytope dimension 4" (crossPolytope 4)
    ,familyTests "cross-polytope dimension 5" (crossPolytope 5)
    ,familyTests "hypersimplex Delta(2,4)" hypersimplex24
    ,familyTests "Birkhoff B3" birkhoff3]
