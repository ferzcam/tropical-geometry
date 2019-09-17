{-# LANGUAGE AllowAmbiguousTypes, DataKinds #-}

--{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
-- {-# LANGUAGE ConstrainedClassMethods, UndecidableInstances, MultiParamTypeClasses #-}


module TPolynomial.THypersurface (testsHypersurface) where


import Test.Tasty
import Test.Tasty.HUnit as HU
import Data.List
import Core
import Data.Map.Strict

x, y :: Polynomial (Tropical Integer) Lex 2
x = variable 0
y = variable 1

f1 = 1*x^2 + x*y + 1*y^2 + x + y + 2
f2 = 3*x^2 + x*y + 3*y^2 + 1*x + 1*y + 0
f3 = 3*x^3 + 1*x^2*y + 1*x*y^2 + 3*y^3 + 1*x^2 + x*y + 1*y^2 + 1*x + 1*y + 3
f4 = x^3 + x^2*y + x*y^2 + y^3 + x^2 + x*y + y^2 + x + y + 0
f5 = 2*x*y^(-1) + 2*y^(-1) + (-2)




testMapTermPoint :: TestTree
testMapTermPoint =   HU.testCase "Get the key-value pair with the terms and its corresponding points" $ do
        show (mapTermPoint f1) @?=  "fromList [((0,0,2),(,2)),((0,1,0),(X_1,0)),((0,2,1),(X_1^2,1)),((1,0,0),(X_0,0)),((1,1,0),(X_0X_1,0)),((2,0,1),(X_0^2,1))]"
 

testFindFanVertex :: TestTree
testFindFanVertex = HU.testCase "Computing fan vertices" $ do
        findFanVertex (mapTermPoint f1) (0,0,2) (0,1,0) (1,0,0) @?= (2,2)


testsHypersurface :: TestTree
testsHypersurface = testGroup "Test for Computing Hypersurfaces" [testMapTermPoint, testFindFanVertex]