{-# LANGUAGE DataKinds #-}

module TPolynomial.TPolytope (testsPolytope) where 


import Test.Tasty
import Test.Tasty.HUnit as HU
import Core
import Data.List

x = variable 0 :: Polynomial (Tropical Integer) Lex 2
y = variable 1 :: Polynomial (Tropical Integer) Lex 2

p1 = 3*x^2*y + 2*x + 3*y + 5 + 6*x^3*y^2 + 8*x^3*y^6 + 3*x^7*y^5

testsPolytope :: TestTree
testsPolytope = HU.testCase "Tests for polytopes of polynomials" $ do
    sort (polytope p1) @?= sort [[0,0],[0,1],[1,0],[7,5],[3,6]]