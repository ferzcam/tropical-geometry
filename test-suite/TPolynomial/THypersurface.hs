{-# LANGUAGE AllowAmbiguousTypes, DataKinds #-}

--{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
-- {-# LANGUAGE ConstrainedClassMethods, UndecidableInstances, MultiParamTypeClasses #-}


module TPolynomial.THypersurface (testsHypersurface) where


import Test.Tasty
import Test.Tasty.HUnit as HU
import Control.Exception (ErrorCall, evaluate, try)
import Data.List
import Core
import Geometry.Polytope (subdivision)
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
        
-- Historical unit-length drawing examples, retained below. The public output
-- uses finite segments to draw unbounded rays; compare their starting points
-- and primitive directions, without imposing a display length of one.
-- testHypersurface :: TestTree
-- testHypersurface = HU.testCase "Compute hypersurface of polynomials" $ do
--         (sort $ hypersurface f4) @?= sort [((0,0), (0,1)), ((0,0), (1,0)), ((0,0), (-1,-1))]
--         (sort $ hypersurface f5) @?= sort [((0,4), (0,5)), ((0,4), (1,4)), ((0,4), (-1, 3))]


-- Only used for these single-vertex fans, whose every edge is an unbounded
-- ray. A zero-length segment retains direction (0,0), so it fails the oracle.
primitiveRay :: ((Int,Int), (Int,Int)) -> ((Int,Int), (Int,Int))
primitiveRay (p@(x,y), (u,v)) =
    let dx = u-x; dy = v-y; divisor = gcd dx dy
    in (p, if divisor == 0 then (0,0) else (dx `div` divisor, dy `div` divisor))

testHypersurfaceF4 :: TestTree
testHypersurfaceF4 = HU.testCase "Zero-coefficient cubic has three tropical rays" $
    sort (map primitiveRay (hypersurface f4))
        @?= sort [((0,0),(0,1)),((0,0),(1,0)),((0,0),(-1,-1))]

-- f5 is min(2+x-y, 2-y, -2). Its three equal terms meet at (0,4);
-- pairwise ties attaining the minimum extend north, east and southwest.
testHypersurfaceF5 :: TestTree
testHypersurfaceF5 = HU.testCase "Laurent triangle has three translated tropical rays" $
    sort (map primitiveRay (hypersurface f5))
        @?= sort [((0,4),(0,1)),((0,4),(1,0)),((0,4),(-1,-1))]

testsHypersurface :: TestTree
testsHypersurface = testGroup "Test for Computing Hypersurfaces" [testMapTermPoint, testFindFanVertex, testInnerNormals, testVerticesNormals, testHypersurfaceF4, testHypersurfaceF5, testsPolygonal]



-- These expected cells/dual edges follow directly from the minimum of the
-- displayed affine terms, independently of the implementation.
testsPolygonal :: TestTree
testsPolygonal = testGroup "Polygonal subdivisions"
    [ HU.testCase "Coplanar square gives four rays and no diagonal" $ do
        let p = 0 + x + y + x*y
        sort (map sort (subdivision p)) @?= [[(0,0),(0,1),(1,0),(1,1)]]
        sort (map primitiveRay (hypersurface p)) @?=
            sort [((0,0),(1,0)),((0,0),(-1,0)),((0,0),(0,1)),((0,0),(0,-1))]
    , HU.testCase "Hexagonal cell has six rays and no diagonal" $ do
        let p = y + x + x^2 + x^3*y + x^2*y^2 + x*y^2
        sort (map sort (subdivision p)) @?= [[(0,1),(1,0),(1,2),(2,0),(2,2),(3,1)]]
        sort (map primitiveRay (hypersurface p)) @?=
            sort [((0,0),(1,1)),((0,0),(0,1)),((0,0),(-1,1)),
                  ((0,0),(-1,-1)),((0,0),(0,-1)),((0,0),(1,-1))]
    , HU.testCase "Inner normals do not overflow their orientation product" $ do
        let m = maxBound `div` 2 :: Int
        innerNormal (0,0) (m,0) (0,m) @?= (0,1)
    , HU.testCase "Collinear normals fail with an explicit diagnostic" $ do
        result <- try (evaluate (innerNormal (0,0) (1,1) (2,2))) :: IO (Either ErrorCall (Int,Int))
        case result of
            Left err -> assertBool "diagnostic explains collinearity" ("collinear" `isInfixOf` show err)
            Right _ -> assertFailure "Expected collinear points to be rejected"
    , HU.testCase "Affine square heights translate its fan" $ do
        let p = 0 + 2*x + 3*y + 5*x*y
        sort (map primitiveRay (hypersurface p)) @?=
            sort [((-2,-3),(1,0)),((-2,-3),(-1,0)),((-2,-3),(0,1)),((-2,-3),(0,-1))]
    , HU.testCase "Adjacent square and triangle have one actual bounded edge" $ do
        let p = 0 + x + y + x*y + 1*x^2
        sort (map sort (subdivision p)) @?=
            sort [[(0,0),(0,1),(1,0),(1,1)],[(1,0),(1,1),(2,0)]]
        sort (hypersurface p) @?= sort
            [((-1,0),(0,0)), ((0,0),(10,0)), ((0,0),(0,10)),
             ((0,0),(0,-10)), ((-1,0),(-1,10)), ((-1,0),(-11,-10))]
    , HU.testCase "Nonintegral fan vertex is rejected instead of truncated" $ do
        let p = 0 + 1*x^2 + y
        result <- try (evaluate (sum [a+b+c+d | ((a,b),(c,d)) <- hypersurface p])) :: IO (Either ErrorCall Int)
        case result of
            Left err -> assertBool "diagnostic explains integral coordinate limitation" ("nonintegral" `isInfixOf` show err)
            Right _ -> assertFailure "Expected explicit rejection of vertex (-1/2,0)"
    , HU.testCase "Normal matching uses exact direction and orientation" $ do
        isInverse (0,0) (2,1) (3,2) (-2,-1) @?= False
        isInverse (0,0) (2,1) (4,2) (-2,-1) @?= True
        isInverse (0,0) (-2,-1) (4,2) (2,1) @?= False
        isInverse (0,0) (2,1) (4,2) (2,1) @?= False
    , HU.testCase "Shared diagonal vertices do not imply cell adjacency" $ do
        let square = [(0,0),(0,2),(2,0),(2,2)]
            triangle = [(0,0),(1,3),(2,2)]
        neighborTriangles [square,triangle] MS.empty @?= MS.fromList [(square,[]),(triangle,[])]
    ]
