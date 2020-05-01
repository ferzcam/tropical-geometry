module TGeometry.TFacet where



import Test.Tasty
import Test.Tasty.HUnit as HU
import Geometry.Facet
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
    (toLists $ fst (generateMatrices set1)) @?= matrix1
    (toLists $ fst (generateMatrices set2)) @?= matrix2
    (toLists $ fst (generateMatrices set3)) @?= matrix3
   


vertices1 = [[0,0],[3,0],[0,3]]
hyperplanes1 = [[0,-1],[-1,0],[1,1]]

vertices2 = [[0,0,0],[0,0,1],[0,1,0],[1,0,0],[0,1,1],[1,0,1],[1,1,0],[1,1,1]]
hyperplanes2 = [[-2,0,0], [0,-2,0], [0,0,-2], [2,0,0], [0,2,0], [0,0,2]]

vertices3 = [[0,0,1],[-1,0,0],[0,-1,0],[1,0,0],[0,1,0],[0,0,-1]]
hyperplanes3 = [[-1,-1,1], [1,-1,1], [1,1,1], [-1,1,1], [-1,-1,-1], [1,-1,-1],[1,1,-1],[-1,1,-1]]

vertices4 = [[0,0,0,1],[-1,0,0,0],[0,-1,0,0],[0,0,-1,0],[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,-1]]
hyperplanes4 = [[-1,-1,-1,1], [-1,-1,1,1], [-1,1,-1,1], [-1,1,1,1], [1,-1,-1,1], [1,-1,1,1],[1,1,-1,1],[1,1,1,1],[-1,-1,1,-1],[-1,1,1,-1],[1,-1,1,-1],[1,1,1,-1],[-1,-1,-1,-1],[-1,1,-1,-1],[1,-1,-1,-1],[1,1,-1,-1]]


testFacetEnum :: TestTree
testFacetEnum =   HU.testCase "Tests for facet enumeration algorithm" $ do
    (sort.(map fromJust).snd.facetEnumeration) vertices1 @?= sort hyperplanes1
    (sort.(map fromJust).snd.facetEnumeration) vertices2 @?= sort hyperplanes2
    (sort.(map fromJust).snd.facetEnumeration) vertices3 @?= sort hyperplanes3
    (sort.(map fromJust).snd.facetEnumeration) vertices4 @?= sort hyperplanes4
  



testsFacet :: TestTree
testsFacet = testGroup "Test for facets" [testAdjacencyMatrix, testFacetEnum]



