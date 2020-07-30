{-# LANGUAGE AllowAmbiguousTypes, DataKinds, NoImplicitPrelude #-}

--{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
-- {-# LANGUAGE ConstrainedClassMethods, UndecidableInstances, MultiParamTypeClasses #-}


module TPolynomial.THypersurface (testsHypersurface, f10) where

import Polynomial.Hypersurface

import Prelude (Integer, ($), (.))
import Test.Tasty
import Test.Tasty.HUnit as HU
import Data.List
import Core
import Data.Maybe
import Numeric.Algebra

import qualified Data.Map.Strict as MS
   

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
f10 = 1*x^2 + x*y + 1*y^2 + x + y + 0


testVertices :: TestTree
testVertices = HU.testCase "Test for vertices of tropical hypersurfaces" $ do
        -- (sort.(MS.keys).vertices) f1 @?= sort [[2,2], [-1,0], [0,0], [0,-1]]
        -- (sort.(MS.keys).vertices) f2 @?= sort [[-1,1], [1,-1], [-2,1], [1,-2]]
        -- (sort.(MS.keys).vertices) f3 @?= sort [[-2,0], [-1,0], [0,1], [1,1], [2,2], [1,0], [0,-1], [0,-2], [-1,-1]]
        -- (sort.(MS.keys).vertices) f4 @?= sort [[0,0]]
        -- (sort.(MS.keys).vertices) f5 @?= sort [[0,4]]
        -- (sort.(MS.keys).vertices) f6 @?= sort [[-4,-3],[-4,-2],[-2,-3],[-1,1],[0,-1],[0,0],[2,-1],[2,0],[5,3]]
        -- (sort.(MS.keys).vertices) f7 @?= sort [[-2,2],[-1,0],[-1,1],[0,0],[2,-3],[2,-1],[2,0],[5,3]]
        -- (sort.(MS.keys).vertices) f8 @?= sort [[-6,-4],[-5,-4],[-4,0],[-2,-4],[-2,2],[-1,0],[-1,1],[0,-3],[0,0],[2,-2],[2,-1],[2,0],[10,8]]
        -- (sort.(MS.keys).vertices) f9 @?= sort [[0,0]]
        -- sort (verticesWithRays f1) @?= sort [V [(-1),0],V [0,(-1)],V [0,0],V [2,2],R [1,0],R [0,1],R [-1,-1]]
        -- sort (verticesWithRays f2) @?= sort [V [(-2),1],V [(-1),1],V [1,(-2)],V [1,(-1)],R [1,0],R [0,1],R [-1,-1]]
        -- sort (verticesWithRays f3) @?= sort [V [(-2),0],V [(-1),(-1)],V [(-1),0],V [0,(-2)],V [0,(-1)],V [0,1],V [1,0],V [1,1],V [2,2],R [1,0],R [0,1],R [-1,-1]]
        -- sort (verticesWithRays f4) @?= sort [V [0,0],R [1,0],R [0,1],R [-1,-1]]
        -- sort (verticesWithRays f5) @?= sort [V [0,4],R [1,0],R [0,1],R [-1,-1]]
        -- sort (verticesWithRays f6) @?= sort [V [(-4),(-3)],V [(-4),(-2)],V [(-2),(-3)],V [(-1),1],V [0,(-1)],V [0,0],V [2,(-1)],V [2,0],V [5,3],R [1,0],R [0,1],R [-1,-1]]
        -- sort (verticesWithRays f7) @?= sort [V [(-2),2],V [(-1),0],V [(-1),1],V [0,0],V [2,(-3)],V [2,(-1)],V [2,0],V [5,3],R [0,1],R [-1,-1],R [1,0]]
        -- sort (verticesWithRays f8) @?= sort [V [(-6),(-4)],V [(-5),(-4)],V [(-4),0],V [(-2),(-4)],V [(-2),2],V [(-1),0],V [(-1),1],V [0,(-3)],V [0,0],V [2,(-2)],V [2,(-1)],V [2,0],V [10,8],R [1,0],R [-1,-1],R [0,1]]
        -- sort (verticesWithRays f9) @?= sort [V [0,0],R [1,0],R [0,1],R [0,-1],R [-1,0]]
        -- sort (verticesWithRays (f10)) @?= sort [V [0,0], V [0,-1], V [-1,0], R [0,1],R [1,0],R [-1,-1]]
        

        MS.map (sort . nub ) (verticesWithRaysIndexed f1) @?=  MS.map (sort) (verticesWithRaysGraph f1)
        MS.map (sort . nub ) (verticesWithRaysIndexed f2) @?=  MS.map (sort) (verticesWithRaysGraph f2)
        MS.map (sort . nub ) (verticesWithRaysIndexed f3) @?=  MS.map (sort) (verticesWithRaysGraph f3)
        MS.map (sort . nub ) (verticesWithRaysIndexed f4) @?=  MS.map (sort) (verticesWithRaysGraph f4)
        MS.map (sort . nub ) (verticesWithRaysIndexed f5) @?=  MS.map (sort) (verticesWithRaysGraph f5)
        MS.map (sort . nub ) (verticesWithRaysIndexed f6) @?=  MS.map (sort) (verticesWithRaysGraph f6)
        MS.map (sort . nub ) (verticesWithRaysIndexed f7) @?=  MS.map (sort) (verticesWithRaysGraph f7)
        MS.map (sort . nub ) (verticesWithRaysIndexed f8) @?=  MS.map (sort) (verticesWithRaysGraph f8)
        MS.map (sort . nub ) (verticesWithRaysIndexed f9) @?=  MS.map (sort) (verticesWithRaysGraph f9)
        MS.map (sort . nub ) (verticesWithRaysIndexed f10) @?= MS.map (sort) (verticesWithRaysGraph f10)


testsHypersurface :: TestTree
testsHypersurface = testGroup "Test for Computing Hypersurfaces" [testVertices] 

