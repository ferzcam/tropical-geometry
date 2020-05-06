module TGeometry.TFacet where


import Geometry.Facet
import Geometry.Vertex
import Util 

import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Data.Matrix
import Data.List
import Data.Maybe

set1 = [[1,5], [2,2], [5,1], [6,3], [6,5]]
set2 = [[0,0,1], [0,1,0], [1,0,0], [1,1,1]]
set3 = [[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0],[1,1,1]]


matrix1 =   [
            [0,1,0,0,1],
            [1,0,1,0,0],
            [0,1,0,1,0],
            [0,0,1,0,1],
            [1,0,0,1,0]    
            ]

matrix2 =   [
            [0,1,1,1],
            [1,0,1,1],
            [1,1,0,1],
            [1,1,1,0]
            ]

matrix3 =   [
            [0,1,1,0,1,0,0,0],
            [1,0,0,1,0,1,0,0],
            [1,0,0,1,0,0,1,0],
            [0,1,1,0,0,0,0,1],
            [1,0,0,0,0,1,1,0],
            [0,1,0,0,1,0,0,1],
            [0,0,1,0,1,0,0,1],
            [0,0,0,1,0,1,1,0]
            ]

testAdjacencyMatrix :: TestTree
testAdjacencyMatrix =   HU.testCase "Tests for generating adjacency matrix for neighbor vertices" $ do
    adjacencyMatrix set1 @?= fromLists matrix1
    adjacencyMatrix set2 @?= fromLists matrix2
    adjacencyMatrix set3 @?= fromLists matrix3
   


vertices1 = [[0,0],[3,0],[0,3]]
facets1 = [[1,3],[1,2],[2,3]]
hyperplanes1 = [[0,-1], [-1,0], [1,1]]
b1 = [0,0,3]
res1 = safeZipWith3 (,,) facets1 hyperplanes1 b1

vertices2 = [[0,0,0],[0,0,1],[0,1,0],[1,0,0],[0,1,1],[1,0,1],[1,1,0],[1,1,1]]
facets2 = [[],[],[],[],[],[]]
hyperplanes2 = [[-2,0,0], [0,-2,0], [0,0,-2], [0,0,2], [0,2,0], [2,0,0]]
b2 = [0,0,0,2,2,2]
res2 = safeZipWith3 (,,) facets2 hyperplanes2 b2


vertices3 = [[0,0,1],[-1,0,0],[0,-1,0],[1,0,0],[0,1,0],[0,0,-1]]
hyperplanes3 = [[-1,-1,1], [1,-1,1], [-1,1,1], [1,1,1], [-1,-1,-1], [-1,1,-1], [1,-1,-1], [1,1,-1]]
b3 = [1,1,1,1,1,1,1,1]
res3 = MS.fromList $ safeZipWith (,) hyperplanes3 b3


vertices4 = [[0,0,0,1],[-1,0,0,0],[0,-1,0,0],[0,0,-1,0],[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,-1]]
hyperplanes4 = [[-1,-1,-1,1], [-1,-1,1,1], [-1,1,-1,1], [-1,1,1,1], [1,-1,-1,1], [1,-1,1,1],[1,1,-1,1],[1,1,1,1],[-1,-1,1,-1],[-1,1,1,-1],[1,-1,1,-1],[1,1,1,-1],[-1,-1,-1,-1],[-1,1,-1,-1],[1,-1,-1,-1],[1,1,-1,-1]]
b4 = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
res4 = MS.fromList $ safeZipWith (,) hyperplanes4 b4


testFacetEnum :: TestTree
testFacetEnum =   HU.testCase "Tests for facet enumeration algorithm" $ do
    facetEnumeration vertices1 @?= res1
    facetEnumeration vertices2 @?= res2
    -- (snd . facetEnumeration) vertices3 @?= (res3)
    -- (snd . facetEnumeration) vertices4 @?= (res4)
  



testsFacet :: TestTree
testsFacet = testGroup "Test for facets" [testAdjacencyMatrix]



