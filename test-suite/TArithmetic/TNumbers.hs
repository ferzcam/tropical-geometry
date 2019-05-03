module TArithmetic.TNumbers (testsNumbers) where

    import Test.Tasty
    import Test.Tasty.HUnit as HU
    import Arithmetic.Numbers
        
    testSumTropicals :: TestTree
    testSumTropicals =   HU.testCase "Sum tropical numbers" $ do
            (Tropical 3) + (Tropical 3)  @?= (Tropical 3 :: Tropical Integer)
            (Inf) + (Tropical 3)  @?= (Tropical 3 :: Tropical Integer)
            (Tropical (-2)) + (Tropical 3)  @?= (Tropical (-2) :: Tropical Integer)

    testProdTropicals :: TestTree
    testProdTropicals =   HU.testCase "Multiply tropical numbers" $ do
            (Tropical 3) * (Tropical 3)  @?= (Tropical 6 :: Tropical Integer)
            (Inf) * (Tropical 3)  @?= (Inf :: Tropical Integer)
            (Tropical (-2)) * (Tropical 3)  @?= (Tropical 1 :: Tropical Integer)
    
    
    testsNumbers :: TestTree
    testsNumbers = testGroup "Test for tropical numbers" [testSumTropicals, testProdTropicals]
    
    
    
    