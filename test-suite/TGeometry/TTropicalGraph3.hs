module TGeometry.TTropicalGraph3 (testsTropicalGraph3) where

import Control.Monad (forM_, unless)
import Data.List (nub, sort)
import Data.Ratio ((%))
import Geometry.TropicalGraph3
import Geometry.TropicalHull3 (hullGraph3)
import Geometry.TropicalSkeleton3
import Geometry.TropicalSlice (Term3(..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

testsTropicalGraph3 :: TestTree
testsTropicalGraph3 = testGroup "Dual-aware 3-variable graph one-skeleton"
    [ testCase "direct, LRS, and tailored hull routes return the same record on integral fixtures" $
        forM_ [simplex,quadratic,splitQuadratic,unequalTerms,integerCubeGrid] $ \terms -> do
            assertEqual "LRS route" (exactGraph3 terms) (lrsGraph3 terms)
            assertEqual "tailored hull route" (exactGraph3 terms) (hullGraph3 terms)
    , testCase "tailored hull route reports the lower cell its GLPK/Yang pipeline misses on the lifted cubic" $ do
        -- The direct and LRS routes find four cells; the legacy facet enumeration
        -- returns only three, so the shared exact assembly must refuse the output
        -- instead of drawing a spurious ray across the interior face.
        assertEqual "LRS route" (exactGraph3 liftedCubic) (lrsGraph3 liftedCubic)
        assertEqual "four cells" (Right 4) (length . graph3Cells <$> exactGraph3 liftedCubic)
        case hullGraph3 liftedCubic of
            Left err -> assertBool ("names the missed cell: " ++ err) ("missed a lower cell" `isInfixOf'` err)
            Right graph -> assertFailure ("hull route returned " ++ show (length (graph3Cells graph)) ++ " cells without error")
    , testCase "direct and LRS routes return the same record on rational fixtures" $
        forM_ [shifted,rationalSplitQuadratic,cubeGrid] $ \terms ->
            assertEqual "LRS route" (exactGraph3 terms) (lrsGraph3 terms)
    , testCase "the record's one-skeleton equals the plain skeleton APIs" $
        forM_ [simplex,shifted,quadratic,splitQuadratic,rationalSplitQuadratic,unequalTerms,liftedCubic,cubeGrid] $ \terms -> do
            assertEqual "exactSkeleton3" (exactSkeleton3 terms) (skeleton3Of <$> exactGraph3 terms)
            assertEqual "lrsTropicalSkeleton3" (lrsTropicalSkeleton3 terms) (skeleton3Of <$> lrsGraph3 terms)
    , testCase "tropical plane has one tetrahedral cell dual to one vertex with four rays" $
        withGraph simplex $ \graph -> do
            assertEqual "one vertex" [[0,0,0]] (map vertex3Point (graph3Vertices graph))
            assertEqual "one cell with all terms" [[0,1,2,3]] (map cell3Terms (graph3Cells graph))
            assertEqual "four faces on the cell" [[0,1,2,3]] (map cell3Faces (graph3Cells graph))
            assertEqual "four rays" 4 (length [() | e <- graph3Edges graph, Ray3 _ _ <- [edge3Geometry e]])
            assertEqual "triangular faces" [3,3,3,3] (map (length . face3Boundary) (graph3Faces graph))
            assertEqual "each face is dual to its own ray" [0..3] (map face3Edge (graph3Faces graph))
    , testCase "fractional simplex vertex is kept exactly with its dual cell" $
        withGraph shifted $ \graph ->
            assertEqual "vertex" [[1%2,2%3,3%4]] (map vertex3Point (graph3Vertices graph))
    , testCase "split quadratic shares one face between two cells, dual to the bounded edge" $
        withGraph splitQuadratic $ \graph -> do
            let shared = [f | f <- graph3Faces graph, length (face3Cells f) == 2]
            assertEqual "one shared face" 1 (length shared)
            let face = head shared
                edge = graph3Edges graph !! face3Edge face
                cells = graph3Cells graph
            assertEqual "dual edge is the bounded segment" (Segment3 [-1,-2,-2] [1,0,0]) (edge3Geometry edge)
            assertEqual "edge joins both cells' vertices" (face3Cells face) (edge3Vertices edge)
            assertEqual "shared face terms are the common cell terms"
                (sort (face3Terms face))
                (sort [i | i <- cell3Terms (cells !! 0), i `elem` cell3Terms (cells !! 1)])
            assertEqual "six boundary faces give six rays" 6
                (length [() | e <- graph3Edges graph, Ray3 _ _ <- [edge3Geometry e]])
    , testCase "paraboloid cube grid has eight cube cells with six quadrilateral faces each" $
        withGraph cubeGrid $ \graph -> do
            assertEqual "eight cells" 8 (length (graph3Cells graph))
            assertEqual "six faces per cell" (replicate 8 6) (map (length . cell3Faces) (graph3Cells graph))
            assertEqual "eight terms per cell" (replicate 8 8) (map (length . cell3Terms) (graph3Cells graph))
            assertEqual "36 faces" 36 (length (graph3Faces graph))
            assertEqual "quadrilateral faces" (replicate 36 4) (map (length . face3Boundary) (graph3Faces graph))
            assertEqual "12 interior faces" 12 (length [() | f <- graph3Faces graph, length (face3Cells f) == 2])
            assertEqual "24 boundary faces" 24 (length [() | f <- graph3Faces graph, length (face3Cells f) == 1])
    , testCase "lifted cubic surface has the slices example's structure" $
        withGraph liftedCubic $ \graph -> do
            assertEqual "ten normalized terms" 10 (length (graph3Terms graph))
            assertBool "has bounded edges" (any isSegment (map edge3Geometry (graph3Edges graph)))
            assertBool "every cell contains the lifted interior term"
                (all (\c -> any (\i -> let t = graph3Terms graph !! i in term3Z t == 1) (cell3Terms c)) (graph3Cells graph))
    , testCase "incidence invariants hold on every route" $
        forM_ [simplex,shifted,quadratic,splitQuadratic,rationalSplitQuadratic,unequalTerms,liftedCubic,cubeGrid] $ \terms -> do
            withGraph terms checkGraph
            either assertFailure checkGraph (lrsGraph3 terms)
    , testCase "duplicate exponents retain their least coefficient on all routes" $ do
        let duplicate = Term3 1 0 0 7 : simplex
        assertEqual "direct" (exactGraph3 simplex) (exactGraph3 duplicate)
        assertEqual "LRS" (exactGraph3 simplex) (lrsGraph3 duplicate)
        assertEqual "hull" (exactGraph3 simplex) (hullGraph3 duplicate)
    , testCase "unsupported inputs are reported by each route rather than redirected" $ do
        forM_ [("direct",exactGraph3),("lrs",lrsGraph3),("hull",hullGraph3)] $ \(name,route) -> do
            assertBool (name ++ " rejects empty input") (isLeft (route []))
            assertBool (name ++ " rejects rank-deficient support") (isLeft (route (take 3 simplex)))
            assertBool (name ++ " rejects more than 32 raw terms") (isLeft (route (replicate 33 (head simplex))))
        case hullGraph3 shifted of
            Left err -> assertBool "hull names its integral contract" ("integral" `isInfixOf'` err)
            Right _ -> assertFailure "the tailored hull route accepted fractional coefficients"
    ]
  where
    simplex = [Term3 0 0 0 0,Term3 1 0 0 0,Term3 0 1 0 0,Term3 0 0 1 0]
    shifted = [Term3 0 0 0 0,Term3 1 0 0 ((-1)%2),Term3 0 1 0 ((-2)%3),Term3 0 0 1 ((-3)%4)]
    quadratic = [Term3 x y z 0 | x <- [0..2],y <- [0..2],z <- [0..2],x+y+z <= 2]
    splitQuadratic = [Term3 0 0 0 0,Term3 1 0 0 (-1),Term3 2 0 0 0,Term3 0 1 0 0,Term3 0 0 1 0]
    rationalSplitQuadratic = [Term3 0 0 0 0,Term3 1 0 0 ((-2)%3),Term3 2 0 0 (1%5),
                              Term3 0 1 0 (1%7),Term3 0 0 1 ((-1)%4)]
    -- min(z, x+z, y+z, 1, 1+3z): the historical regression fixture.
    unequalTerms = [Term3 0 0 1 0,Term3 1 0 1 0,Term3 0 1 1 0,Term3 0 0 0 1,Term3 0 0 3 1]
    -- The slices page's cubic: a genus-one plane cubic with its interior term lifted to z=1.
    liftedCubic = [Term3 0 0 0 0,Term3 1 0 0 1,Term3 0 1 0 1,Term3 2 0 0 4,Term3 1 1 1 3,
                   Term3 0 2 0 4,Term3 3 0 0 9,Term3 2 1 0 7,Term3 1 2 0 7,Term3 0 3 0 9]
    cubeGrid = [Term3 x y z ((x*x+y*y+z*z)%2) | x <- [0..2],y <- [0..2],z <- [0..2]]
    integerCubeGrid = [Term3 x y z (fromInteger (x*x+y*y+z*z)) | x <- [0..2],y <- [0..2],z <- [0..2]]
    isLeft (Left _) = True
    isLeft _ = False
    isSegment (Segment3 _ _) = True
    isSegment _ = False
    isInfixOf' needle haystack = any (needle `prefixOf`) (tails' haystack)
    prefixOf [] _ = True
    prefixOf _ [] = False
    prefixOf (a:as) (b:bs) = a == b && prefixOf as bs
    tails' [] = [[]]
    tails' s@(_:rest) = s : tails' rest

withGraph :: [Term3] -> (Graph3 -> Assertion) -> Assertion
withGraph terms action = either assertFailure action (exactGraph3 terms)

-- Every stated incidence is rechecked against the exact minimum.
checkGraph :: Graph3 -> Assertion
checkGraph graph = do
    let terms = graph3Terms graph
        vertices = graph3Vertices graph
        edges = graph3Edges graph
        cells = graph3Cells graph
        faces = graph3Faces graph
        exponent' t = map fromInteger [term3X t,term3Y t,term3Z t] :: [Rational]
        value t p = term3Coefficient t + sum (zipWith (*) (exponent' t) p)
        active p = let values = map (`value` p) terms in [i | (i,v) <- zip [0..] values, v == minimum values]
        at p d t = zipWith (\x y -> x+t*fromInteger y) p d
    assertEqual "vertex IDs are positional" [0..length vertices-1] (map vertex3Id vertices)
    assertEqual "cells are dual to vertices in order" (map vertex3Id vertices) (map cell3Vertex cells)
    assertEqual "faces are dual to edges in order" (map edge3Id edges) (map face3Edge faces)
    assertEqual "distinct vertices" (length vertices) (length (nub (map vertex3Point vertices)))
    forM_ vertices $ \v ->
        assertEqual ("vertex terms attain the minimum at " ++ show (vertex3Point v))
            (active (vertex3Point v)) (vertex3Terms v)
    forM_ (zip cells vertices) $ \(cell,vertex) -> do
        assertEqual "cell terms equal vertex terms" (vertex3Terms vertex) (cell3Terms cell)
        assertEqual "cell faces list the cell" (cell3Faces cell)
            [face3Id f | f <- faces, cell3Id cell `elem` face3Cells f]
        assertBool "a cell has at least four faces" (length (cell3Faces cell) >= 4)
    forM_ (zip edges faces) $ \(edge,face) -> do
        assertEqual "edge and face share terms" (edge3Terms edge) (face3Terms face)
        assertEqual "edge and face share incident cells" (edge3Vertices edge) (face3Cells face)
        let samples = case edge3Geometry edge of
                Segment3 a b -> [zipWith (\x y -> (x+y)/2) a b,zipWith (\x y -> (2*x+y)/3) a b]
                Ray3 p d -> [at p d (1%3),at p d 5]
                Line3 p d -> [p,at p d 1]
            endpoints = case edge3Geometry edge of
                Segment3 a b -> [a,b]
                Ray3 p _ -> [p]
                Line3 _ _ -> []
        forM_ samples $ \p ->
            assertEqual ("edge terms attain the minimum at " ++ show p) (edge3Terms edge) (active p)
        assertEqual "incident vertices are the endpoints"
            (sort [vertex3Id v | v <- vertices, vertex3Point v `elem` endpoints]) (edge3Vertices edge)
        forM_ (face3Cells face) $ \c ->
            assertBool "face terms belong to each incident cell"
                (all (`elem` cell3Terms (cells !! c)) (face3Terms face))
        assertBool "boundary uses face terms" (all (`elem` face3Terms face) (face3Boundary face))
        assertBool "boundary has at least three corners" (length (face3Boundary face) >= 3)
        assertEqual "boundary corners are distinct" (length (face3Boundary face)) (length (nub (face3Boundary face)))
        let direction = case edge3Geometry edge of
                Segment3 a b -> zipWith (-) b a
                Ray3 _ d -> map fromInteger d
                Line3 _ d -> map fromInteger d
            points = [exponent' (terms !! i) | i <- face3Terms face]
        forM_ points $ \q ->
            assertEqual "face lies in the plane perpendicular to its edge" 0
                (sum (zipWith (*) (zipWith (-) q (head points)) direction))
        unless (null (face3Cells face)) $
            assertBool "an unshared face is dual to a ray; a shared face to a segment"
                (case edge3Geometry edge of
                    Ray3 _ _ -> length (face3Cells face) == 1
                    Segment3 _ _ -> length (face3Cells face) == 2
                    Line3 _ _ -> False)
