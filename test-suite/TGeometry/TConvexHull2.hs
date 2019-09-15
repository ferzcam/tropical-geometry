module TGeometry.TConvexHull2 (testsConvexHull2) where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Geometry.ConvexHull2
import Data.List
       
testConvexHull2 :: TestTree
testConvexHull2 = HU.testCase "Compute convex hull 2D" $ do
        sort (convexHull2 [(3,0),(2,1),(0,3),(0,2),(0,0)]) @?= sort [(3,0),(0,0), (0,3)]
        sort (convexHull2 [(3,0),(2,0),(2,1),(1,1),(1,2),(0,3),(0,2),(1,0),(0,1),(0,0)]) @?= sort [(3,0),(0,0), (0,3)]
        

testsConvexHull2 :: TestTree
testsConvexHull2 = testGroup "Test for convex hull in 2D" [testConvexHull2]


