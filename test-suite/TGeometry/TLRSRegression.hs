module TGeometry.TLRSRegression (testsLRSRegression) where

import Control.Monad (forM_)
import Data.List (nub, permutations, sort, subsequences)
import Data.Matrix (fromLists)
import Data.Ratio ((%))
import Geometry.LRS (colFromList, lrs)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

type Point = [Rational]
type Inequality = (Point, Rational)

-- All fixtures use Ax <= b. Expected vertices are constructed independently
-- from coordinate permutations, cube corners, or simplex Cartesian products.
-- Comparing the raw output also catches duplicate vertices and unstable order.
checkVertices :: [Inequality] -> [Point] -> Point -> Assertion
checkVertices rows expected start = do
    let vertices = lrs (fromLists (map fst rows))
                       (colFromList (map snd rows)) start
        label = "starting vertex " ++ show start
        feasible point = all (\(a, b) -> dot a point <= b) rows
    assertBool (label ++ ": fixture start is feasible") (feasible start)
    assertBool (label ++ ": expected vertices are feasible")
        (all feasible expected)
    assertEqual (label ++ ": exact sorted unique vertices")
        (sort (nub expected)) vertices
    assertBool (label ++ ": every output vertex satisfies Ax <= b")
        (all feasible vertices)

dot :: Point -> Point -> Rational
dot a x = sum (zipWith (*) a x)

atStarts :: String -> [Inequality] -> [Point] -> [Point] -> TestTree
atStarts name rows expected starts = testCase name $
    forM_ starts (checkVertices rows expected)

-- P_n lies in sum x_i = n(n+1)/2. Eliminate x_n and negate
-- the standard subset-sum lower bounds to obtain Ax <= b in R^(n-1).
permutohedron :: Int -> ([Inequality], [Point])
permutohedron n = (rows, vertices)
  where
    total = fromIntegral (n * (n + 1)) / 2
    proper = filter (\s -> not (null s) && length s < n)
        (subsequences [1..n])
    inequality subset =
        let k = length subset
            lower = fromIntegral (k * (k + 1)) / 2
            lastPresent = if n `elem` subset then 1 else 0
            coefficients =
                [lastPresent - (if j `elem` subset then 1 else 0)
                | j <- [1..n-1]]
        in (coefficients, lastPresent * total - lower)
    rows = map inequality proper
    vertices = sort (map (map fromIntegral . take (n-1))
        (permutations [1..n]))

rowOrders :: [(String, [a] -> [a])]
rowOrders =
    [("reversed rows", reverse)
    ,("rotated rows", \xs -> drop 2 xs ++ take 2 xs)
    ,("odd rows before even rows", \xs ->
        [x | (i, x) <- zip [0 :: Int ..] xs, even i] ++
        [x | (i, x) <- zip [0 :: Int ..] xs, odd i])]

rescale :: [Inequality] -> [Inequality]
rescale = zipWith scale (cycle [1%2, 3, 2%3, 5, 7%4])
  where
    scale factor (a, b) = (map (* factor) a, factor * b)

permutohedronTests :: Int -> TestTree
permutohedronTests n = testGroup ("P" ++ show n)
    (atStarts "all starting vertices" rows vertices vertices :
     [atStarts name (order rows) vertices representativeStarts
     | (name, order) <- rowOrders] ++
     [atStarts "positive rational row rescaling" (rescale rows)
        vertices representativeStarts])
  where
    (rows, vertices) = permutohedron n
    representativeStarts = [head vertices, vertices !! (length vertices `div` 2), last vertices]

unit :: Int -> Int -> Point
unit dimension index = [if j == index then 1 else 0 | j <- [0..dimension-1]]

simplex :: Int -> ([Inequality], [Point])
simplex dimension =
    ([(map negate (unit dimension i), 0) | i <- [0..dimension-1]] ++
        [(replicate dimension 1, 1)],
     replicate dimension 0 : [unit dimension i | i <- [0..dimension-1]])

translatedSimplex :: Int -> Point -> TestTree
translatedSimplex dimension translation =
    atStarts ("translated simplex in dimension " ++ show dimension)
        [(a, b + dot a translation) | (a, b) <- rows]
        moved moved
  where
    (rows, vertices) = simplex dimension
    moved = map (zipWith (+) translation) vertices

-- Explicit inverse maps define the facets; forward maps independently define
-- corners. Translation prevents accidental symmetry about the origin from
-- hiding a reversed inequality or coordinate sign.
shearedSquare :: ([Inequality], [Point])
shearedSquare =
    ([([-1,2],4), ([1,-2],-3), ([0,-1],-3), ([0,1],4)],
     [[2 + u + 2*v, 3 + v] | u <- [0,1], v <- [0,1]])

shearedCube :: ([Inequality], [Point])
shearedCube =
    (concatMap (\(a, offset) -> [(map negate a, negate offset), (a, offset+1)])
        [([1,-1,1],6), ([0,1,-1],-4), ([0,0,1],3)],
     [[2 + u + v, -1 + v + w, 3 + w]
     | u <- [0,1], v <- [0,1], w <- [0,1]])

shearTests :: String -> ([Inequality], [Point]) -> TestTree
shearTests name (rows, vertices) = testGroup name
    [atStarts "all starting corners" rows vertices vertices
    ,atStarts "reversed and rescaled rows" (reverse (rescale rows))
        vertices vertices]

simplexProduct :: Int -> Int -> ([Inequality], [Point])
simplexProduct leftDim rightDim =
    ([(a ++ replicate rightDim 0, b) | (a, b) <- leftRows] ++
     [(replicate leftDim 0 ++ a, b) | (a, b) <- rightRows],
     [left ++ right | left <- leftVertices, right <- rightVertices])
  where
    (leftRows, leftVertices) = simplex leftDim
    (rightRows, rightVertices) = simplex rightDim

productTests :: Int -> TestTree
productTests rightDim = testGroup ("simplex product Delta2 x Delta" ++ show rightDim)
    [atStarts "all starting vertices" rows vertices vertices
    ,atStarts "reversed and rescaled rows" (reverse (rescale rows))
        vertices vertices]
  where
    (rows, vertices) = simplexProduct 2 rightDim

-- Twenty-one separately named cases, with multiple starts checked inside each.
-- The test runner should enforce its usual process timeout for nontermination.
testsLRSRegression :: TestTree
testsLRSRegression = testGroup "LRS exact bounded-polytope regressions"
    [permutohedronTests 3
    ,permutohedronTests 4
    ,shearTests "translated sheared square" shearedSquare
    ,shearTests "translated sheared cube" shearedCube
    ,translatedSimplex 2 [2, -3]
    ,translatedSimplex 3 [1%2, -2, 3%4]
    ,translatedSimplex 5 [-2, 1%3, 4, -5%2, 6]
    ,productTests 2
    ,productTests 3]
