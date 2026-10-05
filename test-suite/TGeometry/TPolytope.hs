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

-- A subdivision is a set of triangles, each triangle a set of three
-- vertices: vertex order within a triangle and triangle order in the list
-- are not semantically meaningful. Normalize both before comparing.
normalizeSubdivision :: Ord a => [[a]] -> [[a]]
normalizeSubdivision = sort . map sort

testProjectionToR2 :: TestTree
testProjectionToR2 =   HU.testCase "Project 2D ConvexHull to produce 2D subdivision" $ do
        normalizeSubdivision (projectionToR2 $ fromJust $ convexHull3 newF1) @?= normalizeSubdivision subdivisionF1
        normalizeSubdivision (projectionToR2 $ fromJust $ convexHull3 newF2) @?= normalizeSubdivision subdivisionF2

-- All lifted f4 points are coplanar; their projected extreme triangle is
-- (0,0),(3,0),(0,3). f5 consists of exactly three noncollinear lifted points.
testCoplanarSubdivision :: TestTree
testCoplanarSubdivision = HU.testCase "Subdivision of coplanar lifted triangle" $
    normalizeSubdivision (projectionToR2 $ fromJust $ convexHull3 newF4)
        @?= normalizeSubdivision subdivisionF4

testThreePointSubdivision :: TestTree
testThreePointSubdivision = HU.testCase "Subdivision of three lifted points" $
    normalizeSubdivision (projectionToR2 $ fromJust $ convexHull3 newF5)
        @?= normalizeSubdivision subdivisionF5



testsPolytope :: TestTree
testsPolytope = testGroup "Test for polytopes" [testProjectionToR2, testCoplanarSubdivision, testThreePointSubdivision]
