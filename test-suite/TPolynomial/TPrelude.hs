{-# LANGUAGE AllowAmbiguousTypes, DataKinds, NoImplicitPrelude #-}

{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods, UndecidableInstances, MultiParamTypeClasses #-}


module TPolynomial.TPrelude (testsPrelude) where 

import Prelude (Integer, ($), show)
import Test.Tasty
import Test.Tasty.HUnit as HU
import Data.List
import Core
import Data.Map.Strict
import Numeric.Algebra

testVariables :: TestTree
testVariables = HU.testCase "Creation of variables" $ do
        let x = variable 0 :: Polynomial (Tropical Integer) Lex 3
        show x @?= "X_0"
        let x = variable 1 :: Polynomial (Tropical Integer) Lex 3
        show x @?= "X_1"
        let x = variable 2 :: Polynomial (Tropical Integer) Lex 3
        show x @?= "X_2"


x, y, z :: Polynomial (Tropical Integer) Lex 3
x = variable 0
y = variable 1
z = variable 2

testShowPolynomialLex :: TestTree
testShowPolynomialLex =   HU.testCase "Show polynomial with Lex order" $ do
        show (x*y*z^2) @?= "X_0X_1X_2^2"
        show (x^2+x*y+z) @?= "X_0^2 + X_0X_1 + X_2"
        show ((-3)*x + 0*y^2 + (-1)*x*z) @?= "-1X_0X_2 + -3X_0 + X_1^2"
        show (x+0) @?= "X_0 + 0"


x1, y1, z1 :: Polynomial (Tropical Integer) Revlex 3
x1 = variable 0
y1 = variable 1
z1 = variable 2

testShowPolynomialRevlex :: TestTree
testShowPolynomialRevlex =   HU.testCase "Show polynomial with Revlex order" $ do
        show (x1*y1*z1^2) @?= "X_0^2X_1X_2"
        show (x1^2+x1*y1+z1) @?= "X_0 + X_1X_2 + X_2^2"
        show (-3*x1+y1^2+ (-1)*x1*z1) @?= "-1X_0X_2 + X_1^2 + -3X_2"

testSumProdPolynomial :: TestTree
testSumProdPolynomial = HU.testCase "Sum and product of polynomials" $ do
        x + y*z + (-1)*x @?= (Polynomial $ fromList [(toMonomial [1,0,0], -1), (toMonomial [0,1,1], 0)])  
        x^2 + 2*y*z + (-1)*y + (-1)*y*z @?= (Polynomial $ fromList [(toMonomial [2,0,0], 0), (toMonomial [0,1,0], -1), (toMonomial [0,1,1], -1)])  
        3*x + x @?= x
        3*y^2 + y^2 @?= y^2
        3*y^2 + x + y^2 @?= x + y^2        
        (3*y^2 + x)^2 + 2 @?= 6*y^4 + 3*x*y^2 + x^2 + 2
        3*x*(x+y+z)^2 @?= 3*x^3 + 3*x^2*z + 3*x^2*y + 3*x*y^2 + 3*x*y*z + 3*x*z^2


x2, y2 :: Polynomial (Tropical Integer) Lex 2
x2 = variable 0
y2 = variable 1
p1 = 3*x2^2*y2 + 2*x2 + 3*y2 + 5 + 6*x2^3*y2^2 + 8*x2^3*y2^6 + 3*x2^7*y2^5


testsPrelude :: TestTree
testsPrelude = testGroup "Test for Prelude of Polynomials" [testVariables, testShowPolynomialLex, testShowPolynomialRevlex, testSumProdPolynomial]