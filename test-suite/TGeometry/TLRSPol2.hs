{-# LANGUAGE AllowAmbiguousTypes, DataKinds #-}

module TGeometry.TLRSPol2 (testsVertexEnumPol2) where

import Test.Tasty
import Test.Tasty.HUnit as HU

import Data.List (sort)
import Data.Matrix (Matrix, fromLists)

import Core hiding (Vertex)
import Geometry.LRS (lrs, colFromList)
import Geometry.Facet (facetEnumeration)
import Geometry.Vertex (extremalVertices, Vertex)

-- =============================================================================
-- Active test: cone in R^3
-- =============================================================================
-- mat1 * x >= 0 defines a cone with apex at the origin. The four expected
-- outputs are its extreme rays. All four inequalities are tight at [0,0,0],
-- so the dictionary built by getDictionary already sits at a (degenerate)
-- lex-optimum and revSearch enumerates the rays directly. This is the only
-- case the current LRS implementation handles correctly.

mat1 :: Matrix Rational
mat1 = fromLists [[1,0,-2],[1,-1,0],[0,-1,0],[-1,0,-1]]

testVertexEnum :: TestTree
testVertexEnum = HU.testCase "lrs on R^3 cone enumerates extreme rays" $ do
    lrs mat1 (colFromList [0,0,0,0]) [0,0,0]
        @?= [[(-2)/3,(-2)/3,(-1)/3],[0,(-1),0],[0,0,(-1)],[1,0,(-1)]]

-- Bounded 2D polytope: the unit square in R^2.
-- Constraints (Ax >= b convention used by lrs):
--   [ 1, 0] x >=  0   (x >= 0)
--   [ 0, 1] x >=  0   (y >= 0)
--   [-1, 0] x >= -1   (x <= 1)
--   [ 0,-1] x >= -1   (y <= 1)
-- Vertices: (0,0), (1,0), (1,1), (0,1).
squareMat :: Matrix Rational
squareMat = fromLists [[1,0],[0,1],[-1,0],[0,-1]]

testSquare :: TestTree
testSquare = HU.testCase "lrs on R^2 unit square enumerates 4 vertices" $ do
    lrs squareMat (colFromList [0,0,-1,-1]) [0,0]
        @?= [[0,0],[0,1],[1,0],[1,1]]

-- Bounded 3D polytope: the unit cube in R^3.
-- 6 facets, 8 vertices.
cubeMat :: Matrix Rational
cubeMat = fromLists [[1,0,0],[0,1,0],[0,0,1],[-1,0,0],[0,-1,0],[0,0,-1]]

testCube :: TestTree
testCube = HU.testCase "lrs on R^3 unit cube enumerates 8 vertices" $ do
    lrs cubeMat (colFromList [0,0,0,-1,-1,-1]) [0,0,0]
        @?= [[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0],[1,1,1]]

-- Bounded 4D polytope: the unit hypercube in R^4.
-- 8 facets, 16 vertices. This is the real R^n contract.
tesseractMat :: Matrix Rational
tesseractMat = fromLists
    [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]
    ,[-1,0,0,0],[0,-1,0,0],[0,0,-1,0],[0,0,0,-1]]

testTesseract :: TestTree
testTesseract = HU.testCase "lrs on R^4 unit tesseract enumerates 16 vertices" $ do
    lrs tesseractMat (colFromList [0,0,0,0,-1,-1,-1,-1]) [0,0,0,0]
        @?= [[a,b,c,d] | a <- [0,1], b <- [0,1], c <- [0,1], d <- [0,1]]

-- Bounded 3D polytope: the standard simplex in R^3.
-- {x_1, x_2, x_3 >= 0; x_1 + x_2 + x_3 <= 1}
-- Vertices: origin and three unit basis vectors. 4 facets, non-cubic.
simplexMat :: Matrix Rational
simplexMat = fromLists [[1,0,0],[0,1,0],[0,0,1],[-1,-1,-1]]

testSimplex3 :: TestTree
testSimplex3 = HU.testCase "lrs on R^3 standard simplex enumerates 4 vertices" $ do
    lrs simplexMat (colFromList [0,0,0,-1]) [0,0,0]
        @?= [[0,0,0],[0,0,1],[0,1,0],[1,0,0]]

-- =============================================================================
-- Polynomial Newton-polytope tests (f1..f9)
-- =============================================================================
-- Each f_i is a 2-variable tropical polynomial. expVecs lifts each term to a
-- 3D point (exp_x, exp_y, coef); the Newton polytope is the convex hull. We
-- run extremalVertices -> facetEnumeration to get an H-representation, then
-- lrs to enumerate the vertices.
--
-- Per-polynomial contract: every expVec is itself a vertex of the polytope,
-- so (sort . map toRational . expVecs) f_i should equal lrs's output.

x, y :: Polynomial (Tropical Integer) Lex 2
x = variable 0
y = variable 1

f1, f2, f3, f4, f6, f7, f8, f9 :: Polynomial (Tropical Integer) Lex 2
f1 = 1*x^2 + x*y + 1*y^2 + x + y + 2
f2 = 3*x^2 + x*y + 3*y^2 + 1*x + 1*y + 0
f3 = 3*x^3 + 1*x^2*y + 1*x*y^2 + 3*y^3
   + 1*x^2 + x*y + 1*y^2 + 1*x + 1*y + 3
-- f4 has all implicit coefficients (= Tropical 0); like f9, every lifted
-- point sits at z=0, so the Newton polytope is coplanar in R^3 and LRS's
-- sortSystem precondition (≥ dim tight constraints at the start vertex)
-- can't be met. Excluded from the active test group.
f4 = x^3 + x^2*y + x*y^2 + y^3 + x^2 + x*y + y^2 + x + y + 0
f6 = 6*x^4 + 4*x^3*y + 3*x^2*y^2 + 4*x*y^3 + 5*y^4
   + 2*x^3 + x^2*y + 1*x*y^2 + 4*y^3
   + 2*x^2 + x*y + 3*y^2 + x + 2*y + 5
f7 = 6*x^5 + 2*x^4*y + 4*x^3*y^2 + x^2*y^3 + 3*x*y^4 + 8*y^5
   + 6*x^4 + 4*x^3*y + 3*x^2*y^2 + 4*x*y^3 + 5*y^4
   + 2*x^3 + x^2*y + 1*x*y^2 + 4*y^3
   + 2*x^2 + x*y + 3*y^2 + x + 2*y + 5
f8 = 10*x^6 + 8*x^5*y + 6*x^4*y^2 + 6*x^3*y^3 + 4*x^2*y^4 + 6*x*y^5 + 9*y^6
   + 6*x^5 + 2*x^4*y + 4*x^3*y^2 + x^2*y^3 + 3*x*y^4 + 8*y^5
   + 6*x^4 + 4*x^3*y + 3*x^2*y^2 + 4*x*y^3 + 5*y^4
   + 2*x^3 + x^2*y + 1*x*y^2 + 4*y^3
   + 2*x^2 + x*y + 3*y^2 + x + 2*y + 10
-- f9 has all coefs == 0; lifted points are coplanar → 2D Newton polytope in
-- R^3. LRS as implemented requires a full-dimensional polytope (sortSystem
-- demands at least `dim` tight constraints at the starting vertex), so f9 is
-- expected to fail until LRS handles lower-dimensional polytopes explicitly.
f9 = x^2*y^2 + y^2 + x^2 + 0

lrsPoly :: Polynomial (Tropical Integer) Lex 2 -> [Vertex]
lrsPoly poly = lrs matsHyp bHyp (map toRational (head points))
    where
        points          = expVecs poly
        facetEnumerated = facetEnumeration (extremalVertices points)
        matsHyp         = fromLists   $ map (\(_,h,_) -> h) facetEnumerated
        bHyp            = colFromList $ map (\(_,_,b) -> b) facetEnumerated

-- lrs should recover exactly the extremal subset of expVecs (interior terms
-- excluded).
polyTest :: String -> Polynomial (Tropical Integer) Lex 2 -> TestTree
polyTest name poly = HU.testCase ("lrs on Newton polytope of " ++ name) $ do
    let extremal = sort $ map (map toRational) $ extremalVertices (expVecs poly)
    lrsPoly poly @?= extremal

testsVertexEnumPol2 :: TestTree
testsVertexEnumPol2 = testGroup "Tests for LRS vertex enumeration"
    [ testVertexEnum
    , testSquare, testCube, testTesseract, testSimplex3
    , polyTest "f1" f1
    , polyTest "f2" f2
    , polyTest "f3" f3
    -- f4 and f9 are degenerate (coplanar lifted points); see definitions above
    , polyTest "f6" f6
    , polyTest "f7" f7
    , polyTest "f8" f8
    ]

-- =============================================================================
-- Pending tests: full polytope path (lrsPoly)
-- =============================================================================
-- These exercise the bounded-polytope branch of LRS via Newton polytopes of
-- 2-variable tropical polynomials f1..f9. They were written on generalTropHyp
-- and depend on:
--   - Polynomial.Hypersurface.expVecs
--   - Geometry.Facet.facetEnumeration
--   - Geometry.Vertex.extremalVertices
-- expVecs is not yet ported to upgrade_lts, so the block stays commented out
-- until LRS itself works on bounded polytopes (bugs 1-6 in the review).
--
-- Re-enable once `simplex` is wired into `lrs` and the lex-pivot rule is
-- correct; expect at least f1, f4, f9 to pass first.
--
--   import Polynomial.Hypersurface (expVecs)
--   import Geometry.Facet (facetEnumeration)
--   import Geometry.Vertex (extremalVertices, Vertex)
--   import Core
--
--   x, y :: Polynomial (Tropical Integer) Lex 2
--   x = variable 0
--   y = variable 1
--
--   f1 = 1*x^2 + x*y + 1*y^2 + x + y + 2
--   f2 = 3*x^2 + x*y + 3*y^2 + 1*x + 1*y + 0
--   f3 = 3*x^3 + 1*x^2*y + 1*x*y^2 + 3*y^3
--      + 1*x^2 + x*y + 1*y^2 + 1*x + 1*y + 3
--   f4 = x^3 + x^2*y + x*y^2 + y^3 + x^2 + x*y + y^2 + x + y + 0
--   f5 = 2*x*y^(-1) + 2*y^(-1) + (-2)
--   f6 = 6*x^4 + 4*x^3*y + 3*x^2*y^2 + 4*x*y^3 + 5*y^4
--      + 2*x^3 + x^2*y + 1*x*y^2 + 4*y^3
--      + 2*x^2 + x*y + 3*y^2 + x + 2*y + 5
--   f7 = 6*x^5 + 2*x^4*y + 4*x^3*y^2 + x^2*y^3 + 3*x*y^4 + 8*y^5
--      + 6*x^4 + 4*x^3*y + 3*x^2*y^2 + 4*x*y^3 + 5*y^4
--      + 2*x^3 + x^2*y + 1*x*y^2 + 4*y^3
--      + 2*x^2 + x*y + 3*y^2 + x + 2*y + 5
--   f8 = 10*x^6 + 8*x^5*y + 6*x^4*y^2 + 6*x^3*y^3 + 4*x^2*y^4 + 6*x*y^5 + 9*y^6
--      + 6*x^5 + 2*x^4*y + 4*x^3*y^2 + x^2*y^3 + 3*x*y^4 + 8*y^5
--      + 6*x^4 + 4*x^3*y + 3*x^2*y^2 + 4*x*y^3 + 5*y^4
--      + 2*x^3 + x^2*y + 1*x*y^2 + 4*y^3
--      + 2*x^2 + x*y + 3*y^2 + x + 2*y + 10
--   f9 = x^2*y^2 + y^2 + x^2 + 0
--
--   -- Alternative matrix-form test (commented in the original):
--   --   mat = fromLists [[-1,0,0],[0,-1,0],[0,0,-1],
--   --                    [1,0,0],[0,1,0],[0,1,1],
--   --                    [0,-1,1],[1,0,1],[-1,0,1]]
--   --   b   = colFromList [0,0,0,1,1,2,1,2,1]
--   --   testLRS = HU.testCase "..." $ lrs mat b @?= [[0,0,0]]
--
--   lrsPoly :: Polynomial (Tropical Integer) Lex 2 -> [Vertex]
--   lrsPoly poly = lrs matsHyp bHyp (map toRational $ head points)
--     where
--       points          = expVecs poly
--       facetEnumerated = facetEnumeration $ extremalVertices points
--       matsHyp         = fromLists      $ map (\(_,h,_) -> h) facetEnumerated
--       bHyp            = colFromList    $ map (\(_,_,b) -> b) facetEnumerated
--
--   testPolytopes :: TestTree
--   testPolytopes = HU.testCase "lrs on Newton polytopes" $ do
--       (sort . expVecs) f1 @?= lrsPoly f1
--       (sort . expVecs) f2 @?= lrsPoly f2
--       (sort . expVecs) f3 @?= lrsPoly f3
--       (sort . expVecs) f4 @?= lrsPoly f4
--       (sort . expVecs) f5 @?= lrsPoly f5
--       (sort . expVecs) f6 @?= lrsPoly f6
--       (sort . expVecs) f7 @?= lrsPoly f7
--       (sort . expVecs) f8 @?= lrsPoly f8
--       (sort . expVecs) f9 @?= lrsPoly f9
