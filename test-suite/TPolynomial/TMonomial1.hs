module TPolynomial.TMonomial1 (testsMonomial1) where 


import Test.Tasty
import Test.Tasty.HUnit as HU
import Polynomial.Monomial1

testShowMonomial1 :: TestTree
testShowMonomial1 =   HU.testCase "Show monomial xy^2z^3" $ do
        show (Monomial [1,2,3]) @?= "X_0X_1^2X_2^3"

testShowMonomial2 ::   TestTree
testShowMonomial2 =   HU.testCase "Show monomial xz^3" $
        show (Monomial [1,0,3]) @?= "X_0X_2^3"

testShowMonomial3 ::   TestTree
testShowMonomial3 =   HU.testCase "Show monomial x" $
        show (Monomial [1,0,0]) @?= "X_0"

testShowMonomial4 ::   TestTree
testShowMonomial4 =   HU.testCase "Show monomial ()" $
        show (Monomial [0,0,0]) @?= ""

testShowMonomial :: TestTree
testShowMonomial = testGroup "Test for showing monomials" [testShowMonomial1, testShowMonomial2, testShowMonomial3, testShowMonomial4]


testCompareMonomial1 :: TestTree
testCompareMonomial1 = HU.testCase "Compare Lex Monomials x < y^5z^4" $
        (Monomial [1,0,0] :: Monomial Lex) < (Monomial [0,5,4] :: Monomial Lex) @?= False 

testCompareMonomial2 :: TestTree
testCompareMonomial2 = HU.testCase "Compare Lex Monomials y < x^5z^4" $
        (Monomial [0,1,0] :: Monomial Lex) < (Monomial [1,0,4] :: Monomial Lex) @?= True 

testCompareMonomial3 :: TestTree
testCompareMonomial3 = HU.testCase "Compare Revlex Monomials x < y^5z^4" $
        (Monomial [1,0,0] :: Monomial Revlex) < (Monomial [0,5,4] :: Monomial Revlex) @?= True 

testCompareMonomial4 :: TestTree
testCompareMonomial4 = HU.testCase "Compare Revlex Monomials y^2 < xz^4" $
        (Monomial [0,2,0] :: Monomial Revlex) < (Monomial [1,0,4] :: Monomial Revlex) @?= True
                

testCompareMonomial :: TestTree
testCompareMonomial = testGroup "Test for comparing monomials" [testCompareMonomial1, testCompareMonomial2, testCompareMonomial3, testCompareMonomial4]

testsMonomial1 :: TestTree
testsMonomial1 = testGroup "Test for monomials" [testShowMonomial, testCompareMonomial]



