{-# LANGUAGE DataKinds #-}

module TPolynomial.TMonomial (testsMonomial) where 


import Test.Tasty
import Test.Tasty.HUnit as HU
import Polynomial.Monomial1
import Polynomial.Prelude1
import GHC.TypeLits

-- testShowMonomial1 :: TestTree
-- testShowMonomial1 =   HU.testCase "Show monomial xy^2z^3" $ do
--         show (Monomial [1,2,3]) @?= "X_0X_1^2X_2^3"

-- testShowMonomial2 ::   TestTree
-- testShowMonomial2 =   HU.testCase "Show monomial xz^3" $
--         show (Monomial [1,0,3]) @?= "X_0X_2^3"

-- testShowMonomial3 ::   TestTree
-- testShowMonomial3 =   HU.testCase "Show monomial x" $
--         show (Monomial [1,0,0]) @?= "X_0"

-- testShowMonomial4 ::   TestTree
-- testShowMonomial4 =   HU.testCase "Show monomial ()" $
--         show (Monomial [0,0,0]) @?= ""

-- testShowMonomial :: TestTree
-- testShowMonomial = testGroup "Test for showing monomials" [testShowMonomial1, testShowMonomial2, testShowMonomial3, testShowMonomial4]


-- testCompareMonomial1 :: TestTree
-- testCompareMonomial1 = HU.testCase "Compare Lex Monomials x < y^5z^4" $
--         (Monomial [1,0,0] :: Monomial Lex) < (Monomial [0,5,4] :: Monomial Lex) @?= False 

-- testCompareMonomial2 :: TestTree
-- testCompareMonomial2 = HU.testCase "Compare Lex Monomials y < x^5z^4" $
--         (Monomial [0,1,0] :: Monomial Lex) < (Monomial [1,0,4] :: Monomial Lex) @?= True 

-- testCompareMonomial3 :: TestTree
-- testCompareMonomial3 = HU.testCase "Compare Revlex Monomials x < y^5z^4" $
--         (Monomial [1,0,0] :: Monomial Revlex) < (Monomial [0,5,4] :: Monomial Revlex) @?= True 

-- testCompareMonomial4 :: TestTree
-- testCompareMonomial4 = HU.testCase "Compare Revlex Monomials y^2 < xz^4" $
--         (Monomial [0,2,0] :: Monomial Revlex) < (Monomial [1,0,4] :: Monomial Revlex) @?= True
                

-- testCompareMonomial :: TestTree
-- testCompareMonomial = testGroup "Test for comparing monomials" [testCompareMonomial1, testCompareMonomial2, testCompareMonomial3, testCompareMonomial4]

-- testsMonomial :: TestTree
-- testsMonomial = testGroup "Test for monomials" [testShowMonomial, testCompareMonomial]

-- x, y, z :: Polynomial (Tropical Integer) Lex 3

x = variable 1 3
y = variable 2 3
z = variable 3 3

testShowMonomial1 :: TestTree
testShowMonomial1 =   HU.testCase "Show monomial xy^2z^3" $ do
        show (x*y^2*z^3) @?= "X_0X_1^2X_2^3"

testShowMonomial2 ::   TestTree
testShowMonomial2 =   HU.testCase "Show monomial xz^3" $
        show (x*y^3) @?= "X_0X_2^3"

testShowMonomial3 ::   TestTree
testShowMonomial3 =   HU.testCase "Show monomial x" $
        show (x) @?= "X_0"

testShowMonomial4 ::   TestTree
testShowMonomial4 =   HU.testCase "Show monomial ()" $
        show (1 :: Monomial Lex 3) @?= ""

testShowMonomial :: TestTree
testShowMonomial = testGroup "Test for showing monomials" [testShowMonomial1, testShowMonomial2, testShowMonomial3, testShowMonomial4]


testCompareMonomial1 :: TestTree
testCompareMonomial1 = HU.testCase "Compare Lex Monomials x < y^5z^4" $
        (x :: Monomial Lex 3) < (y^5*z^4 :: Monomial Lex 3) @?= False 

testCompareMonomial2 :: TestTree
testCompareMonomial2 = HU.testCase "Compare Lex Monomials y < x^5z^4" $
        (y :: Monomial Lex) < (x*z^4 :: Monomial Lex) @?= True 

testCompareMonomial3 :: TestTree
testCompareMonomial3 = HU.testCase "Compare Revlex Monomials x < y^5z^4" $
        (x :: Monomial Revlex) < (y^5*z^4 :: Monomial Revlex) @?= True 

testCompareMonomial4 :: TestTree
testCompareMonomial4 = HU.testCase "Compare Revlex Monomials y^2 < xz^4" $
        (y^2 :: Monomial Revlex) < (x*z^4 :: Monomial Revlex) @?= True
                

testCompareMonomial :: TestTree
testCompareMonomial = testGroup "Test for comparing monomials" [testCompareMonomial1, testCompareMonomial2, testCompareMonomial3, testCompareMonomial4]

testsMonomial :: TestTree
testsMonomial = testGroup "Test for monomials" [testShowMonomial, testCompareMonomial]



