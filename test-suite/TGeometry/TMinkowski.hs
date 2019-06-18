module TGeometry.TMinkowski (testsMinkowski) where

    import Test.Tasty
    import Test.Tasty.HUnit as HU
    import Geometry.Minkowski
    import Data.List      
    
    testMinkowskiSum :: TestTree
    testMinkowskiSum = HU.testCase "Compute Minkowski Sum" $ do
            minkowskiSum2D [] [] @?= []
            minkowskiSum3D [] [] @?= []
            minkowskiSum2D [(0,-1), (0,1), (1,0)] [(0,0), (1,-1), (1,1)] @?= sort [(1,0), (2,1), (2,-1), (0,1), (1,2), (1,0), (0,-1), (1,0), (1,-2)]


    testsMinkowski :: TestTree
    testsMinkowski = testGroup "Test for minkowskisum" [testMinkowskiSum]
    
    
    