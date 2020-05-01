module TGeometry.TVertex where



import Test.Tasty
import Test.Tasty.HUnit as HU
import Geometry.Vertex


set1 = [[5,1], [6,3], [6,5], [1,5], [2,2], [5,2], [3,4]]
set2 = [[1,0,0], [0,1,0], [0,0,1], [1,1,1], [0.5, 0.5, 0.5]]
testExtremeVertex :: TestTree
testExtremeVertex =   HU.testCase "Tests for checking extreme vertices in a set" $ do
    isExtremeVertex (set1!!0) set1 @?= True
    isExtremeVertex (set1!!1) set1 @?= True
    isExtremeVertex (set1!!2) set1 @?= True
    isExtremeVertex (set1!!3) set1 @?= True
    isExtremeVertex (set1!!4) set1 @?= True
    isExtremeVertex (set1!!5) set1 @?= False
    isExtremeVertex (set1!!6) set1 @?= False
    isExtremeVertex (set2!!0) set2 @?= True
    isExtremeVertex (set2!!1) set2 @?= True
    isExtremeVertex (set2!!2) set2 @?= True
    isExtremeVertex (set2!!3) set2 @?= True
    isExtremeVertex (set2!!4) set2 @?= False


testOneFace :: TestTree
testOneFace = HU.testCase "Tests for checking 1-faces in a set" $ do
    isOneFace [5,1] [6,3] set1 @?= True
    isOneFace [5,1] [6,5] set1 @?= False
    isOneFace [5,1] [5,2] set1 @?= False
    isOneFace [1,5] [2,2] set1 @?= True
    isOneFace [1,0,0] [0,1,0] set2 @?= True
    isOneFace [1,0,0] [0.5,0.5,0.5] set2 @?= False
testsSimplex :: TestTree
testsSimplex = testGroup "Test for polytopes" [testExtremeVertex, testOneFace]



