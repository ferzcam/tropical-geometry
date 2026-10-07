{-# LANGUAGE AllowAmbiguousTypes, DataKinds, NoImplicitPrelude #-}

--{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
-- {-# LANGUAGE ConstrainedClassMethods, UndecidableInstances, MultiParamTypeClasses #-}


module TPolynomial.THypersurface3 (testsHypersurface3) where

import Polynomial.Hypersurface

import Prelude (Integer, ($), (.))
import Test.Tasty
import Test.Tasty.HUnit as HU
import Data.List
import Data.Ratio ((%))
import Core
import Data.Maybe
import Numeric.Algebra

import qualified Data.Map.Strict as MS

x, y, z :: Polynomial (Tropical Integer) Lex 3
x = variable 0
y = variable 1
z = variable 2

f1 = 3*x*y*z + x + y + 2*z + (-2)
f2 = x + y + z + 1 

f3 = 1*y^2 + (-1)*x^2 + (-1)*z^2 + (-1)*x^3

-- The shared 2-face normals have opposite directions but unequal scales.
f4 = x*z + y*z + z + 1 + 1*z^3


testVertices3 :: TestTree
testVertices3 = HU.testCase "Test for vertices of tropical hypersurfaces" $ do
        -- (sort.(MS.keys).vertices) f1 @?= sort [[-2,-2,-1],[-2,1,-4],[1,-2,-4]]
        -- (sort.(MS.keys).vertices) f2 @?= sort [[1,1,1]]
        -- (sort.(MS.keys).vertices) f3 @?= sort [[0,-1,0]]
        -- sort (verticesWithRays f1) @?= sort [V [(-2),(-2),(-1)],V [(-2),1,(-4)],V [1,(-2),(-4)],R [0,0,1],R [-1,-1,1],R [0,1,0],R [-1,1,-1],R [1,0,0],R [1,-1,-1]]
        -- sort (verticesWithRays f2) @?= sort [V [1,1,1],R [1,0,0],R [0,1,0],R [0,0,1],R [-1,-1,-1]]
        -- sort (verticesWithRays f3) @?= sort [V [0,(-1),0],R [1,1,1],R [0,1,0],R [-2,-3,-3],R [0,0,1]]
        MS.map (sort . nub ) (verticesWithRaysIndexed f1) @?=  MS.map (sort) (verticesWithRaysGraph f1)
        MS.map (sort . nub ) (verticesWithRaysIndexed f2) @?=  MS.map (sort) (verticesWithRaysGraph f2)
        MS.map (sort . nub ) (verticesWithRaysIndexed f3) @?=  MS.map (sort) (verticesWithRaysGraph f3)

testWrapperRegressions :: TestTree
testWrapperRegressions = testGroup "Historical wrapper regressions"
        [testRayDirectionsAreNotVertices, testScaledFacetNormals]

testRayDirectionsAreNotVertices :: TestTree
testRayDirectionsAreNotVertices = HU.testCase "Ray directions are not reported as vertices" $ do
        let (_, simplex) = hypersurface f2
        sort (vertHyp simplex) @?= [[1,1,1]]
        sort (MS.findWithDefault [] [1,1,1] (rays simplex)) @?=
            sort [[1,0,0],[0,1,0],[0,0,1],[-1,-1,-1]]

testScaledFacetNormals :: TestTree
testScaledFacetNormals = HU.testCase "Opposite facet normals with unequal scales identify one bounded edge" $ do
        let (_, unequal) = hypersurface f4
        sort (vertHyp unequal) @?= sort [[0,0,(-1) % 2],[0,0,1]]
        case [edge | edge <- edHyp unequal, isInternal edge] of
            [Internal (a,b)] -> sort [a,b] @?= sort [[0,0,(-1) % 2],[0,0,1]]
            _ -> HU.assertFailure "Expected exactly one bounded edge between the two tropical vertices"
        sort (MS.findWithDefault [] [0,0,(-1) % 2] (rays unequal)) @?=
            sort [[1,0,0],[0,1,0],[-2,-2,-1]]
        sort (MS.findWithDefault [] [0,0,1] (rays unequal)) @?=
            sort [[1,0,0],[0,1,0],[-1,-1,1]]

testsHypersurface3 :: TestTree
testsHypersurface3 = testGroup "Test for Computing Hypersurfaces 3" [testVertices3, testWrapperRegressions]

