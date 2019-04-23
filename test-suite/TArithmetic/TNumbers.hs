module TArithmetic.TNumbers (testsNumbers) where

    import Test.Tasty
    import Test.Tasty.HUnit as HU
    import Arithmetic.Numbers
        
    testSumTropicals1 :: TestTree
    testSumTropicals1 =   HU.testCase "3+3 = 3" $ do
            (Tropical 3) + (Tropical 3)  @?= (Tropical 3 :: Tropical Integer)

    testSumTropicals2 :: TestTree
    testSumTropicals2 =   HU.testCase "Inf+3 = 3" $ do
            (Inf) + (Tropical 3)  @?= (Tropical 3 :: Tropical Integer)
 
    testSumTropicals3 :: TestTree
    testSumTropicals3 =   HU.testCase "-2+3 = -2" $ do
            (Tropical (-2)) + (Tropical 3)  @?= (Tropical (-2) :: Tropical Integer)


    testProdTropicals1 :: TestTree
    testProdTropicals1 =   HU.testCase "3*3 = 6" $ do
            (Tropical 3) * (Tropical 3)  @?= (Tropical 6 :: Tropical Integer)

    testProdTropicals2 :: TestTree
    testProdTropicals2 =   HU.testCase "Inf*3 = Inf" $ do
            (Inf) * (Tropical 3)  @?= (Inf :: Tropical Integer)
    
    testProdTropicals3 :: TestTree
    testProdTropicals3 =   HU.testCase "-2*3 = 1" $ do
            (Tropical (-2)) * (Tropical 3)  @?= (Tropical 1 :: Tropical Integer)
        

    testSumTropical :: TestTree
    testSumTropical = testGroup "Test for summing tropical numbers" [testSumTropicals1, testSumTropicals2, testSumTropicals3]

    testProdTropical :: TestTree
    testProdTropical = testGroup "Test for multiplying tropical numbers" [testProdTropicals1, testProdTropicals2, testProdTropicals3]


    testsNumbers :: TestTree
    testsNumbers = testGroup "Test for tropical numbers" [testSumTropical, testProdTropical]
    
    
    
    