{-# LANGUAGE AllowAmbiguousTypes, DataKinds #-}

--{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
-- {-# LANGUAGE ConstrainedClassMethods, UndecidableInstances, MultiParamTypeClasses #-}


module TPolynomial.THypersurface (testsHypersurface) where


import Test.Tasty
import Test.Tasty.HUnit as HU
import Data.List
import Core
import qualified Data.Map.Strict as MS 

x, y :: Polynomial (Tropical Integer) Lex 2
x = variable 0
y = variable 1

f1 = 1*x^2 + x*y + 1*y^2 + x + y + 2
f2 = 3*x^2 + x*y + 3*y^2 + 1*x + 1*y + 0
f3 = 3*x^3 + 1*x^2*y + 1*x*y^2 + 3*y^3 + 1*x^2 + x*y + 1*y^2 + 1*x + 1*y + 3
f4 = x^3 + x^2*y + x*y^2 + y^3 + x^2 + x*y + y^2 + x + y + 0
f5 = 2*x*y^(-1) + 2*y^(-1) + (-2)


x1, y1 :: Polynomial (Tropical (Expr Integer)) Lex 2
x1 = variable 0
y1 = variable 1


a,b,c,d,e,f,g,h,i :: Tropical (Expr Integer)

a = Tropical $ fromString("a")
b = Tropical $ fromString("b")
c = Tropical $ fromString("c")
d = Tropical $ fromString("d")
e = Tropical $ fromString("e")
f = Tropical $ fromString("f")
g = Tropical $ fromString("g")
h = Tropical $ fromString("h")
i = Tropical $ fromString("i")


f6 = a!*x1^b*y1^c + d!*x1^e*y1^f + g!*x1^h*y1^i

--f6 = a*x1^b*y1^c + d*x1^2e*y1^f + g*x1^h*y1^i

testMapTermPoint :: TestTree
testMapTermPoint =   HU.testCase "Get the key-value pair with the terms and its corresponding points" $ do
        show (mapTermPoint f1) @?=  "fromList [((0,0),(,2)),((0,1),(X_1,0)),((0,2),(X_1^2,1)),((1,0),(X_0,0)),((1,1),(X_0X_1,0)),((2,0),(X_0^2,1))]"
 

testFindFanVertex :: TestTree
testFindFanVertex = HU.testCase "Computing fan vertices" $ do
        findFanVertex (mapTermPoint f1) [(0,0), (0,1), (1,0)] @?= ((2,2), sort [(-1,-1),(1,0),(0,1)])
        findFanVertex (mapTermPoint f1) [(1,1), (0,1), (1,0)] @?= ((0,0), sort [(1,1),(-1,0),(0,-1)])
        findFanVertex (mapTermPoint f1) [(2,0), (1,1), (1,0)] @?= ((-1,0),sort [(-1,-1),(1,0),(0,1)])
        findFanVertex (mapTermPoint f1) [(0,2), (0,1), (1,1)] @?= ((0,-1),sort [(-1,-1),(1,0),(0,1)])

testInnerNormals :: TestTree
testInnerNormals = HU.testCase "Compute inner normals of triangles" $ do
        sort (innerNormals (0,0) (0,1) (1,0)) @?= sort [(-1,-1), (1,0), (0,1)]

testVerticesNormals :: TestTree
testVerticesNormals = HU.testCase "Test for vertices" $ do
        -- (verticesNormals f1) @?= MS.fromList [ ((2,2), sort [(-1,-1),(1,0),(0,1)]), 
        --                                         ((0,0), sort [(1,1),(-1,0),(0,-1)]),
        --                                         ((-1,0),sort [(-1,-1),(1,0),(0,1)]),
        --                                         ((0,-1),sort [(-1,-1),(1,0),(0,1)])]
        (verticesNormals f6) @?= MS.fromList []
        --sort (vertices f2) @?= sort [(-2,1),(-1,1),(1,-1),(1,-2)]
        --sort (vertices f3) @?= sort [(-2,0),(-1,0),(0,1),(1,1),(2,2),(1,0),(0,-1),(0,-2),(-1,-1)]
        --sort (vertices f4) @?= sort [(0,0)]
        --sort (vertices f5) @?= sort [(0,4)]
        

testsHypersurface :: TestTree
testsHypersurface = testGroup "Test for Computing Hypersurfaces" [testMapTermPoint, testFindFanVertex, testInnerNormals, testVerticesNormals]

