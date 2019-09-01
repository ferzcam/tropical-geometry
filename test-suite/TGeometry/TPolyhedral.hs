module TGeometry.TPolyhedral (testsPolyhedral) where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Geometry.ConvexHull
import Geometry.Polyhedral
import Data.List
import Data.Maybe

-- | Should be equal to [(0,0,0),(0,4,0),(4,0,0),(0,0,4),(4,4,0),(0,4,4),(4,0,4),(4,4,4)]

cube = fromJust $ convexHull3D [(1,1,1),(0,0,0),(3,3,3),(0,4,0),(4,0,0),(2,0,2),(2,2,2),(0,0,4),(4,4,0),(0,4,4),(4,0,4),(4,4,4)]
facetsPoint444 = map fromVertices [[(4,4,4), (4,4,0), (0,4,0), (0,4,4)], [(4,4,4), (0,4,4), (0,0,4), (4,0,4)], [(4,4,4), (4,0,4), (4,0,0), (4,4,0)]]


testAdjacentFacets :: TestTree
testAdjacentFacets = HU.testCase "Tests for adjacent facets" $ do
    adjacentFacets (4,4,4) cube @?= facetsPoint444

testsPolyhedral :: TestTree
testsPolyhedral = testGroup "Test for computing polyhedral algorithms" [testAdjacentFacets]
