{-# LANGUAGE AllowAmbiguousTypes, DataKinds #-}



module TGeometry.TLRSPol2 (testsVertexEnumPol2) where

import Polynomial.Hypersurface

import Test.Tasty
import Test.Tasty.HUnit as HU
import Data.List
import Core
import Data.Maybe

import Data.Matrix
   

x, y :: Polynomial (Tropical Integer) Lex 2
x = variable 0
y = variable 1

f1 = 1*x^2 + x*y + 1*y^2 + x + y + 2
f2 = 3*x^2 + x*y + 3*y^2 + 1*x + 1*y + 0
f3 = 3*x^3 + 1*x^2*y + 1*x*y^2 + 3*y^3 + 1*x^2 + x*y + 1*y^2 + 1*x + 1*y + 3
f4 = x^3 + x^2*y + x*y^2 + y^3 + x^2 + x*y + y^2 + x + y + 0

f5 = 2*x*y^(-1) + 2*y^(-1) + (-2)


f6 = 6*x^4 + 4*x^3*y + 3*x^2*y^2 + 4*x*y^3 + 5*y^4 + 2*x^3 + x^2*y + 1*x*y^2 + 4*y^3 + 2*x^2 + x*y + 3*y^2 + x + 2*y + 5
f7 = 6*x^5 + 2*x^4*y + 4*x^3*y^2 + x^2*y^3 + 3*x*y^4 + 8*y^5 + 6*x^4 + 4*x^3*y + 3*x^2*y^2 + 4*x*y^3 + 5*y^4 + 2*x^3 + x^2*y + 1*x*y^2 + 4*y^3 + 2*x^2 + x*y + 3*y^2 + x + 2*y + 5

f8 = 10*x^6 + 8*x^5*y + 6*x^4*y^2 + 6*x^3*y^3 + 4*x^2*y^4 + 6*x*y^5 + 9*y^6 + 6*x^5 + 2*x^4*y + 4*x^3*y^2 + x^2*y^3 + 3*x*y^4 + 8*y^5 + 6*x^4 + 4*x^3*y + 3*x^2*y^2 + 4*x*y^3 + 5*y^4 + 2*x^3 + x^2*y + 1*x*y^2 + 4*y^3 + 2*x^2 + x*y + 3*y^2 + x + 2*y + 10
f9 = x^2*y^2 + y^2 + x^2 + 0


mat = fromLists [
                        [-1,0,0],
                        [0,-1,0],
                        [0,0,-1],
                        [1,0,0],
                        [0,1,0],
                        [0,1,1],
                        [0,-1,1],
                        [1,0,1],
                        [-1,0,1]
                    ]

b = colFromList [0,0,0,1,1,2,1,2,1]


lrsPoly :: Polynomial (Tropical Integer) Lex 2 -> [Vertex]
lrsPoly poly = lrs matsHyp bHyp (map toRational $ head points)
    where
        points = expVecs poly
        facetEnumerated = facetEnumeration $ extremalVertices points
        matsHyp = fromLists $ map (\(_,h,_) -> h) facetEnumerated
        bHyp = colFromList $ map (\(_,_,b) -> b) facetEnumerated

mat1 = (fromLists [[1,0,-2],[1,-1,0],[0,-1,0],[-1,0,-1]])

-- testLRS :: TestTree
-- testLRS = HU.testCase "Test for LRS in two variable polys" $ do
--         (lrs mat b) @?= [[0,0,0]]


testVertexEnum :: TestTree
testVertexEnum = HU.testCase "Test for LRS in two variable polys" $ do
        lrs mat1 (colFromList [0,0,0,0]) [0,0,0] @?= [[(-2)/3,(-2)/3,(-1)/3],[0,(-1),0],[0,0,(-1)],[1,0,(-1)]]
--        (sort.expVecs) f1 @?= lrsPoly f1
        -- (sort.expVecs) f2 @?= lrsPoly f2
        -- (sort.expVecs) f3 @?= lrsPoly f3
  --      (sort.expVecs) f4 @?= lrsPoly f4
 --       (sort.expVecs) f5 @?= lrsPoly f5
        -- (sort.expVecs) f6 @?= lrsPoly f6
        -- (sort.expVecs) f7 @?= lrsPoly f7
        -- (sort.expVecs) f8 @?= lrsPoly f8
        -- (sort.expVecs) f9 @?= lrsPoly f9

testsVertexEnumPol2 :: TestTree
testsVertexEnumPol2 = testGroup "Test for LRS in two variable polys" [] 

--testVertexEnum,