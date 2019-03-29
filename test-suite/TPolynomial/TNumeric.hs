module TPolynomial.TNumeric (testsNumeric) where 


    import Test.Tasty
    import Test.Tasty.HUnit as HU
    import Core
    import Data.Map.Strict as MS

    poly1_1 :: Polynomial (Tropical Integer) Lex
    poly1_1 = Polynomial $ MS.fromList[(Monomial [1,0,0], 3)]

    poly1_2 :: Polynomial (Tropical Integer) Lex
    poly1_2 = variable 1 3
    
    testSumPolys1 :: TestTree
    testSumPolys1 =   HU.testCase "Sum 3x + x = x" $ do
            poly1_1 + poly1_2 @?= (Polynomial $ MS.fromList[(Monomial [1,0,0], 0)])
    
    poly2_1 :: Polynomial (Tropical Integer) Lex
    poly2_1 = Polynomial $ MS.fromList[(Monomial [0,2,0], 3)]

    poly2_2 :: Polynomial (Tropical Integer) Lex
    poly2_2 = variable 2 3
    
    testSumPolys2 :: TestTree
    testSumPolys2 =   HU.testCase "Sum 3y^2 + y^2 = y^2" $ do
            poly2_1 + poly2_2*poly2_2 @?= (Polynomial $ fromList[(Monomial [0,2,0], 0)])
    
    testSumPolys :: TestTree
    testSumPolys = testGroup "Test for summing polynomials" [testSumPolys1, testSumPolys2]
    
    testsNumeric :: TestTree
    testsNumeric = testGroup "Test for monomials" [testSumPolys]
    
    
    
    