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
f5 = 2*x*y^^(-1) + 2*y^^(-1) + (-2)


-- Normals 
-- (ne (northEast): means that the diagonal component points to north-east direction)
-- (nw (northWest): means that the diagonal component points to north-west direction)
-- (se (southEast): means that the diagonal component points to south-east direction)
-- (sw (southWest): means that the diagonal component points to south-west direction)


ne = sort [(1,1), (-1, 0), (0, -1)]
nw = sort [(-1,1), (1, 0), (0, -1)]
se = sort [(1,-1), (-1, 0), (0, 1)]
sw = sort [(-1,-1), (1, 0), (0, 1)]


testMapTermPoint :: TestTree
testMapTermPoint =   HU.testCase "Get the key-value pair with the terms and its corresponding points" $ do
        show (mapTermPoint f1) @?=  "fromList [((0,0),(,2)),((0,1),(X_1,0)),((0,2),(X_1^2,1)),((1,0),(X_0,0)),((1,1),(X_0X_1,0)),((2,0),(X_0^2,1))]"
 

testFindFanVertex :: TestTree
testFindFanVertex = HU.testCase "Computing fan vertices" $ do
        findFanNVertex (mapTermPoint f1) [(0,0), (0,1), (1,0)] @?= ((2,2), sw)
        findFanNVertex (mapTermPoint f1) [(1,1), (0,1), (1,0)] @?= ((0,0), ne)
        findFanNVertex (mapTermPoint f1) [(2,0), (1,1), (1,0)] @?= ((-1,0),sw)
        findFanNVertex (mapTermPoint f1) [(0,2), (0,1), (1,1)] @?= ((0,-1),sw)
        findFanNVertex (mapTermPoint f5) [(1,-1), (0,-1), (0,0)] @?= ((0,4), sw)

testInnerNormals :: TestTree
testInnerNormals = HU.testCase "Compute inner normals of triangles" $ do
        sort (innerNormals (0,0) (0,1) (1,0)) @?= sort [(-1,-1), (1,0), (0,1)]

testVerticesNormals :: TestTree
testVerticesNormals = HU.testCase "Test for vertices and their normals" $ do
        (verticesNormals f1) @?= MS.fromList [ ((2,2), sw), ((0,0), ne), ((-1,0), sw), ((0,-1), sw)]
        (verticesNormals f2) @?= MS.fromList [((-2,1), sw), ((-1,1), se), ((1,-1), nw), ((1,-2), sw)]
        (verticesNormals f3) @?= MS.fromList [((-2,0), sw), ((-1,0), ne), ((0,1), sw), ((1,1), ne), ((2,2), sw), ((1,0), sw), ((0,-1), ne), ((0,-2), sw), ((-1,-1), sw)]
        (verticesNormals f4) @?= MS.fromList [((0,0), sw)]
        (verticesNormals f5) @?= MS.fromList [((0,4), sw)]
        
-- testHypersurface :: TestTree
-- testHypersurface = HU.testCase "Compute hypersurface of polynomials" $ do
--         (sort $ hypersurface f4) @?= sort [((0,0), (0,1)), ((0,0), (1,0)), ((0,0), (-1,-1))]
--         (sort $ hypersurface f5) @?= sort [((0,4), (0,5)), ((0,4), (1,4)), ((0,4), (-1, 3))]


testsHypersurface :: TestTree
testsHypersurface = testGroup "Test for Computing Hypersurfaces" [testMapTermPoint, testFindFanVertex, testInnerNormals, testVerticesNormals] 

