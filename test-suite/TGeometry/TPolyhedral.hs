module TGeometry.TPolyhedral (testsPolyhedral) where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Geometry.ConvexHull3
import Geometry.Polyhedral
import Data.List
import Data.Maybe

-- | Should be equal to [(0,0,0),(0,4,0),(4,0,0),(0,0,4),(4,4,0),(0,4,4),(4,0,4),(4,4,4)]

cube = fromJust $ convexHull3 [(1,1,1),(0,0,0),(3,3,3),(0,4,0),(4,0,0),(2,0,2),(2,2,2),(0,0,4),(4,4,0),(0,4,4),(4,0,4),(4,4,4)]
facetsPoint444 = map fromVertices [[(4,4,4), (4,4,0), (0,4,0), (0,4,4)], [(4,4,4), (0,4,4), (0,0,4), (4,0,4)], [(4,4,4), (4,0,4), (4,0,0), (4,4,0)]]

facet1 = fromVertices [(4,4,4), (4,4,0), (0,4,0), (0,4,4)]
facet2 = fromVertices [(4,4,4), (0,4,4), (0,0,4), (4,0,4)]
facet3 = fromVertices [(4,4,4), (4,0,4), (4,0,0), (4,4,0)]


testNormalVector :: TestTree
testNormalVector = HU.testCase "Tests for normal vector" $ do
    normalVector (Vertex (4,4,4)) facet1 @?= (0,16,0)
    normalVector (Vertex (4,4,4)) facet2 @?= (0,0,16)
    normalVector (Vertex (4,4,4)) facet3 @?= (16,0,0)

testNormalCone :: TestTree
testNormalCone = HU.testCase "Tests for normal cone" $
    normalCone (Vertex (4,4,4)) [facet1, facet2, facet3] @?= [(256,0,0),(0,256,0),(0,0,256)]
-- At (4,4,4), exactly the cube faces x=4, y=4 and z=4 meet.
-- Compare facet membership independent of edge/list order. This assertion
-- does not specify the clockwise/counterclockwise ordering contract.
testAdjacentFacets :: TestTree
testAdjacentFacets = HU.testCase "Cube vertex belongs to exactly three facets" $
    sort (map (sort . fromFacet) (adjacentFacets (4,4,4) cube))
        @?= sort (map (sort . fromFacet) facetsPoint444)

testsPolyhedral :: TestTree
testsPolyhedral = testGroup "Test for computing polyhedral algorithms" [testNormalVector, testNormalCone, testAdjacentFacets]
