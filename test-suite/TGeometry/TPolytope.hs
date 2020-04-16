module TGeometry.TPolytope where



import Test.Tasty
import Data.List
import Test.Tasty.HUnit as HU
import Data.Maybe

import Geometry.Polytope
import Geometry.ConvexHull3

newF1, newF2 :: [(Int, Int, Int)]
newF1 = [(2,0,3), (1,1,0), (0,2,3), (1,0,1),(0,1,1), (0,0,0)]
newF2 = [(3,0,3), (2,1,1), (1,2,1), (0,3,3), (2,0,1), (1,1,0), (0,2,1), (1,0,1), (0,1,1), (0,0,3)]
newF4 = [(3,0,0), (2,1,0), (1,2,0), (0,3,0), (2,0,0), (1,1,0), (0,2,0), (1,0,0), (0,1,0), (0,0,0)]
newF5 = [(1,-1,2), (0,-1,2), (0,0,-2)]

subdivisionF1 = [[(0,0),(1,1),(1,0)],[(0,2),(1,1),(0,1)],[(1,1),(0,0),(0,1)],[(1,1),(2,0),(1,0)]]
subdivisionF2 = [[(3,0),(2,0),(2,1)],[(2,1),(2,0),(1,1)],[(1,2),(2,1),(1,1)],[(0,3),(1,2),(0,2)],[(1,2),(1,1),(0,2)],[(1,1),(2,0),(1,0)],[(0,2),(1,1),(0,1)],[(1,1),(1,0),(0,1)],[(0,1),(1,0),(0,0)]]
subdivisionF4 = [[(3,0), (0,0), (0,3)]]
subdivisionF5 = [[(1,-1), (0,-1), (0,0)]]

testProjectionToR2 :: TestTree
testProjectionToR2 =   HU.testCase "Project 2D ConvexHull to produce 2D subdivision" $ do
        sort (projectionToR2 $ fromJust $ convexHull3 newF1) @?= sort subdivisionF1
--        sort (projectionToR2 $ fromJust $ convexHull3 newF2) @?= sort subdivisionF2
        --sort (projectionToR2 $ fromJust $ convexHull3 newF4) @?= sort subdivisionF4
--        sort (projectionToR2 $ fromJust $ convexHull3 newF5) @?= sort subdivisionF5



testsPolytope :: TestTree
testsPolytope = testGroup "Test for polytopes" [testProjectionToR2]



