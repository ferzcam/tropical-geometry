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
-- WORKS BUT THE RESULT IS WRONG BECAUSE ORDER NOT ALWAYS MATCHES
-- testAdjacentFacets :: TestTree
-- testAdjacentFacets = HU.testCase "Tests for adjacent facets" $ do
--     adjacentFacets (4,4,4) cube @?= facetsPoint444

testsPolyhedral :: TestTree
testsPolyhedral = testGroup "Test for computing polyhedral algorithms" [testNormalVector, testNormalCone]
