{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods, UndecidableInstances, MultiParamTypeClasses #-}


module TPolynomial.TPrelude (testsPrelude) where 


import Test.Tasty
import Test.Tasty.HUnit as HU
import Core
import Data.Map.Strict

testVariable1 :: TestTree
testVariable1 = HU.testCase "Creation of variables" $ do
        let x = variable 1 3 :: Polynomial (Tropical Integer) Lex
        show x @?= "X_0"

testVariable2 :: TestTree
testVariable2 = HU.testCase "Creation of variables" $ do
        let x = variable 2 3 :: Polynomial (Tropical Integer) Lex
        show x @?= "X_1"

testVariable3 :: TestTree
testVariable3 = HU.testCase "Creation of variables" $ do
        let x = variable 3 3 :: Polynomial (Tropical Integer) Lex
        show x @?= "X_2"

testVariables :: TestTree
testVariables = testGroup "Tests for creation of variables" [testVariable1, testVariable2, testVariable3]


x, y, z :: Polynomial (Tropical Integer) Lex

x = variable 1 3
y = variable 2 3
z = variable 3 3

testShowPolynomial1 :: TestTree
testShowPolynomial1 =   HU.testCase "Show polynomial xyz^2 (Lex)" $
        show (x*y*z^2) @?= "X_0X_1X_2^2"
    
testShowPolynomial2 :: TestTree
testShowPolynomial2 =   HU.testCase "Show polynomial x^2 + xy + z (Lex)" $
        show (x^2+x*y+z) @?= "X_0^2 + X_0X_1 + X_2"

testShowPolynomial3 :: TestTree
testShowPolynomial3 =   HU.testCase "Show polynomial -3x + y^2 + (-1)xz (Lex)" $
        show ((-3)*x + 0*y^2 + (-1)*x*z) @?= "-1X_0X_2 + -3X_0 + X_1^2"


x1, y1, z1 :: Polynomial (Tropical Integer) Revlex

x1 = variable 1 3
y1 = variable 2 3
z1 = variable 3 3
        

testShowPolynomial4 :: TestTree
testShowPolynomial4 =   HU.testCase "Show polynomial xyz^2 (Revlex)" $
        show (x1*y1*z1^2) @?= "X_0^2X_1X_2"
    
testShowPolynomial5 :: TestTree
testShowPolynomial5 =   HU.testCase "Show polynomial x^2 + xy + z (Revlex)" $
        show (x1^2+x1*y1+z1) @?= "X_0 + X_1X_2 + X_2^2"

testShowPolynomial6 :: TestTree
testShowPolynomial6 =   HU.testCase "Show polynomial -3x + y^2 - xz (Revlex)" $
        show (-3*x1+y1^2+ (-1)*x1*z1) @?= "-1X_0X_2 + X_1^2 + -3X_2"

testsShowPolynomial :: TestTree
testsShowPolynomial = testGroup "Test for showing polynomials" [testShowPolynomial1, testShowPolynomial2, testShowPolynomial3, testShowPolynomial4, testShowPolynomial5, testShowPolynomial6]


testSumProdPolynomial1 :: TestTree
testSumProdPolynomial1 = HU.testCase "Sum polynomials (x + yz) - (x)" $
        x + y*z + (-1)*x @?= (Polynomial $ fromList [(Monomial [1,0,0], -1), (Monomial [0,1,1], 0)])  

testSumProdPolynomial2 :: TestTree
testSumProdPolynomial2 = HU.testCase "Sum polynomials (x^2 + 2yz) - (y + yz)" $
        x^2 + 2*y*z + (-1)*y + (-1)*y*z @?= (Polynomial $ fromList [(Monomial [2,0,0], 0), (Monomial [0,1,0], -1), (Monomial [0,1,1], -1)])  
        
testsSumProdPolynomial :: TestTree
testsSumProdPolynomial = testGroup "Test for some addition and multiplication between polynomials" [testSumProdPolynomial1, testSumProdPolynomial2]


testSumPolys1 :: TestTree
testSumPolys1 =   HU.testCase "Sum 3x + x" $ do
        3*x + x @?= x

testSumPolys2 :: TestTree
testSumPolys2 =   HU.testCase "Sum 3y^2 + y^2" $ do
        3*y^2 + y^2 @?= y^2

testSumPolys3 :: TestTree
testSumPolys3 =   HU.testCase "Sum 3y^2 + x + y^2" $ do
        3*y^2 + x + y^2 @?= x + y^2


testProdPolys1 :: TestTree
testProdPolys1 = HU.testCase "Prod (3y^2 + x)^2" $ do
        (3*y^2 + x)^2 + 2 @?= 6*y^4 + 3*x*y^2 + x^2 + 2
 

testProdPolys2 :: TestTree
testProdPolys2 =   HU.testCase "Prod 3x(x+y+z)^2" $ do
        3*x*(x+y+z)^2 @?= 3*x^3 + 3*x^2*z + 3*x^2*y + 3*x*y^2 + 3*x*y*z + 3*x*z^2


testSumPolys :: TestTree
testSumPolys = testGroup "Test for summing polynomials" [testSumPolys1, testSumPolys2, testSumPolys3]

testProdPolys :: TestTree
testProdPolys = testGroup "Test for multiplying polynomials" [testProdPolys1, testProdPolys2]

testsPrelude :: TestTree
testsPrelude = testGroup "Test for Prelude of Polynomials" [testVariables, testsShowPolynomial, testsSumProdPolynomial, testSumPolys, testProdPolys]