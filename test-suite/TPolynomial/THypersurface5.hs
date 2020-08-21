{-# LANGUAGE AllowAmbiguousTypes, DataKinds, NoImplicitPrelude #-}

--{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
-- {-# LANGUAGE ConstrainedClassMethods, UndecidableInstances, MultiParamTypeClasses #-}


module TPolynomial.THypersurface5 (testsHypersurface5) where

import Polynomial.Hypersurface

import Prelude (Integer, ($), (/), (.))
import Test.Tasty
import Test.Tasty.HUnit as HU
import Data.List
import Core
import Data.Maybe
import Numeric.Algebra hiding ((/))

   
import qualified Data.Map.Strict as MS

v, w, x, y, z :: Polynomial (Tropical Integer) Lex 5
v = variable 0
w = variable 1
x = variable 2
y = variable 3
z = variable 4


f1 = (x^2 + y^2 + (-1)*z^2)*(v^2+w^2+z^2 + (-1))



testVertices5 :: TestTree
testVertices5 = HU.testCase "Test for vertices of tropical hypersurfaces" $ do
      -- sort (verticesWithRays f1) @?= sort [V [(-1)/2,(-1)/2,(-1),(-1),(-1)/2],R [1,0,0,0,0],R [0,1,0,0,0],R [0,0,1,1,1],R [0,0,1,0,0],R [0,0,0,1,0],R [-1,-1,-1,-1,-1],R [0,0,-1,-1,0]]
      MS.map (sort . nub ) (verticesWithRaysIndexed f1) @?=  MS.map (sort) (verticesWithRaysGraph f1)
       
testsHypersurface5 :: TestTree
testsHypersurface5 = testGroup "Test for Computing Hypersurfaces 5" [testVertices5] 

