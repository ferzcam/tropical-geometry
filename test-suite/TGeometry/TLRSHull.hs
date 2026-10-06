module TGeometry.TLRSHull (testsLRSHull) where

import Data.List (sort)
import Data.Ratio ((%))
import Geometry.LRSHull
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

square :: [[Rational]]
square = [[x,y] | x <- [0,1], y <- [0,1]]

cube :: [[Rational]]
cube = [[x,y,z] | x <- [0,1], y <- [0,1], z <- [0,1]]

boxFacets :: Int -> [HullFacet]
boxFacets dimension = sort $ concat
    [[(replace i (-1),0),(replace i 1,1)] | i <- [0..dimension-1]]
  where replace i x = [if j == i then x else 0 | j <- [0..dimension-1]]

testsLRSHull :: TestTree
testsLRSHull = testGroup "Exact polar LRS hull"
    [ testCase "unit square supporting edges" $
        lrsHull square @?= Right (boxFacets 2)
    , testCase "cube has six polygonal facets" $
        lrsHull cube @?= Right (boxFacets 3)
    , testCase "tetrahedron supporting planes" $
        lrsHull [[0,0,0],[1,0,0],[0,1,0],[0,0,1]] @?=
            Right (sort [([-1,0,0],0),([0,-1,0],0),([0,0,-1],0),([1,1,1],1)])
    , testCase "duplicate interior and coplanar facet points preserve cube" $
        lrsHull (cube ++ cube ++ [[1/2,1/2,1/2],[0,1/2,1/2],[1,1/2,0]]) @?=
            Right (boxFacets 3)
    , testCase "rational shifted rectangle canonical planes" $
        lrsHull [[x,y] | x <- [1%3,5%6], y <- [-2%5,3%5]] @?=
            Right (sort [([-3,0],-1),([6,0],5),([0,-5],2),([0,5],3)])
    , testCase "translated sheared square retains outward orientation" $
        lrsHull [[2+x+y,-3+y] | [x,y] <- square] @?=
            Right (sort [([-1,1],-5),([1,-1],6),([0,-1],3),([0,1],-2)])
    , testCase "4D simplex bounded polar" $
        lrsHull [[0,0,0,0],[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]] @?=
            Right (sort [([-1,0,0,0],0),([0,-1,0,0],0),([0,0,-1,0],0),([0,0,0,-1],0),([1,1,1,1],1)])
    , testCase "input order does not affect polar facets" $
        lrsHull (reverse cube) @?= lrsHull cube
    , testCase "prepared seed is tight independent vertex not polar origin" $
        case prepareHull square of
            Left err -> error err
            Right prepared@(_,rows,start) -> do
                assertBool "seed lies in polar" (all (\row -> sum (zipWith (*) row start) <= 1) rows)
                length (filter (\row -> sum (zipWith (*) row start) == 1) rows) @?= 2
                enumerateHull prepared @?= boxFacets 2
    , testCase "reject rank deficient and malformed input" $
        map (either (const True) (const False) . lrsHull)
            [[], [[]], [[0,0],[1,1]], [[0,0,0],[1,0,0],[0,1,0]], [[0,0],[1]], [[0],[1]]] @?=
            replicate 6 True
    ]
