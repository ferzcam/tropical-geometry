{-# LANGUAGE AllowAmbiguousTypes, DataKinds #-}

--{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
-- {-# LANGUAGE ConstrainedClassMethods, UndecidableInstances, MultiParamTypeClasses #-}


module TPolynomial.THypersurface3 (testsHypersurface3) where

import Polynomial.Hypersurface


import Test.Tasty
import Test.Tasty.HUnit as HU
import Data.List
import Core
import Data.Maybe
   

import qualified Data.Map.Strict as MS

x, y, z :: Polynomial (Tropical Integer) Lex 3
x = variable 0
y = variable 1
z = variable 2

f1 = 3*x*y*z + x + y + 2*z + (-2)
f2 = x + y + z + 1 

f3 = 1*y^2 + (-1)*x^2 + (-1)*z^2 + (-1)*x^3


testVertices3 :: TestTree
testVertices3 = HU.testCase "Test for vertices of tropical hypersurfaces" $ do
        -- (sort.(MS.keys).vertices) f1 @?= sort [[-2,-2,-1],[-2,1,-4],[1,-2,-4]]
        -- (sort.(MS.keys).vertices) f2 @?= sort [[1,1,1]]
        -- (sort.(MS.keys).vertices) f3 @?= sort [[0,-1,0]]
        verticesWithRays f1 @?= [V [(-2),(-2),(-1)],V [(-2),1,(-4)],V [1,(-2),(-4)],R [1,0,-1],R [0,1,-1],R [0,0,1],R [-1,-1,1],R [1,-1,0],R [0,1,0],R [0,-1,1],R [-1,1,-1],R [1,0,0],R [-1,1,0],R [-1,0,1],R [1,-1,-1]]
        verticesWithRays f2 @?= [V [1,1,1],R [1,0,0],R [0,1,0],R [0,0,1],R [-1,-1,-1]]
        verticesWithRays f3 @?= [V [0,(-1),0],R [1,1,1],R [0,1,0],R [-2,-3,-3],R [0,0,1]]
        
testsHypersurface3 :: TestTree
testsHypersurface3 = testGroup "Test for Computing Hypersurfaces" [testVertices3] 

