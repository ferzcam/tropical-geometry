module TGeometry.TConvexHull2 (testsConvexHull2) where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Geometry.ConvexHull2
import Data.List
       
testConvexHull2 :: TestTree
testConvexHull2 = HU.testCase "Compute convex hull 2D" $ do
        sort (convexHull2 [(3,0),(2,1),(0,3),(0,2),(0,0)]) @?= sort [(3,0),(0,0), (0,3)]
        sort (convexHull2 [(3,0),(2,0),(2,1),(1,1),(1,2),(0,3),(0,2),(1,0),(0,1),(0,0)]) @?= sort [(3,0),(0,0), (0,3)]
        sort (convexHull2 [(1,3),(2,7),(3,1),(4,6),(4,-1),(5,2),(6,4),(7,1),(8,6),(9,8),(10,2),(11,-1),(12,2),(13,5)]) @?= sort [(1,3),(2,7),(9,8),(13,5),(11,-1),(4,-1)]

testsConvexHull2 :: TestTree
testsConvexHull2 = testGroup "Test for convex hull in 2D" [testConvexHull2]


