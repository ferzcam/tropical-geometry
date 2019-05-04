{-# LANGUAGE DataKinds #-}

module TPolynomial.TMonomial (testsMonomial) where 


import Test.Tasty
import Test.Tasty.HUnit as HU
import Polynomial.Monomial
import Polynomial.Prelude
import GHC.TypeLits

testShowMonomial :: TestTree
testShowMonomial =   HU.testCase "ShowingMonomials" $ do
        show (toMonomial [1,2,3] :: Monomial Lex 3) @?= "X_0X_1^2X_2^3"
        show (toMonomial [1,0,3] :: Monomial Lex 3) @?= "X_0X_2^3"
        show (toMonomial [1,0,0] :: Monomial Lex 3) @?= "X_0"
        show (toMonomial [0,0,0] :: Monomial Lex 3) @?= ""


testCompareMonomial :: TestTree
testCompareMonomial = HU.testCase "Compare Lex and Revlex Monomials " $ do
        (toMonomial [1,0,0] :: Monomial Lex 3) < (toMonomial [0,5,4] :: Monomial Lex 3) @?= False 
        (toMonomial [0,1,0] :: Monomial Lex 3) < (toMonomial [5,0,4] :: Monomial Lex 3) @?= True 
        (toMonomial [1,0,0] :: Monomial Revlex 3) < (toMonomial [0,5,4] :: Monomial Revlex 3) @?= True 
        (toMonomial [0,2,0] :: Monomial Revlex 3) < (toMonomial [1,0,4] :: Monomial Revlex 3) @?= True


testsMonomial :: TestTree
testsMonomial = testGroup "Test for monomials" [testShowMonomial, testCompareMonomial]



