module TPolynomial.TPolytope (testsPolytope) where 


import Test.Tasty
import Test.Tasty.HUnit as HU
import Core



testsPolytope :: TestTree
testsVariable = HU.testCase "Tests for polyotpe of polynomials" $ do
    let x = variable 0 :: Polynomial (Tropical Integer) Lex 3
    show x @?= "X_0"


    testProdPolys :: TestTree
    testProdPolys = testGroup "Test for multiplying polynomials" [testProdPolys1, testProdPolys2]
    
    testsPrelude :: TestTree
    testsPrelude = testGroup "Test for Prelude of Polynomials" [testVariables, testsShowPolynomial, testsSumProdPolynomial, testSumPolys, testProdPolys]
