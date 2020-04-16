module TGeometry.TFacet where



import Test.Tasty
import Test.Tasty.HUnit as HU
import Geometry.Facet
import Data.Matrix

set1 = [[1,5], [2,2], [5,1], [6,3], [6,5]]
set2 = [[0,0,1], [0,1,0], [1,0,0], [1,1,1]]

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

testAdjacencyMatrix :: TestTree
testAdjacencyMatrix =   HU.testCase "Tests for generating adjacency matrix for neighbor vertices" $ do
    generateMatrix set1 @?= matrix1
    generateMatrix set2 @?= matrix2
    
testsFacet :: TestTree
testsFacet = testGroup "Test for facets" [testAdjacencyMatrix]



