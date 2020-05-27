{-# LANGUAGE AllowAmbiguousTypes, DataKinds #-}

--{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
-- {-# LANGUAGE ConstrainedClassMethods, UndecidableInstances, MultiParamTypeClasses #-}


module TPolynomial.THypersurface4 (testsHypersurface4) where

import Polynomial.Hypersurface

import Test.Tasty
import Test.Tasty.HUnit as HU
import Data.List
import Core
import Data.Maybe
   
import qualified Data.Map.Strict as MS

w, x, y, z :: Polynomial (Tropical Integer) Lex 4
w = variable 0
x = variable 1
y = variable 2
z = variable 3


f1 = 3*x*w + 2*x^2*w^2 + (-5)*x*y + z + 10*x^2*y*w + y*z + (-3)



testVertices4 :: TestTree
testVertices4 = HU.testCase "Test for vertices of tropical hypersurfaces" $ do
      verticesWithRays f1 @?= [V [(-9)/2,2,0,(-3)],V [8,(-23),0,(-28)],R [1,-1,1,0],R [1,0,0,0],R [-1,1,0,0],R [-1,1,-1,1],R [1,-2,0,-2],R [1,-1,0,-1],R [0,-1,0,-2],R [-1,2,0,2]]
        --(sort.(MS.keys).vertices) f1 @?= sort [[8,-23,0,-28],[-9/2,2,0,-3]]
      --  (sort.(map fromJust).vertices) f2 @?= sort []
        
testsHypersurface4 :: TestTree
testsHypersurface4 = testGroup "Test for Computing Hypersurfaces" [testVertices4] 

