module TGeometry.TConvexHull3 (testsConvexHull3) where

    import Test.Tasty
    import Test.Tasty.HUnit as HU
    import Geometry.ConvexHull3
    import Data.List
      
    
    testComputeSegment :: TestTree
    testComputeSegment = HU.testCase "Compute segments" $ do
            computeSegment [] @?= Nothing
            computeSegment [(1,2,3)] @?= Nothing
            computeSegment [(1,2,3), (1,2,3)] @?= Nothing
            computeSegment [(1,2,3),(2,4,6),(7,6,5),(3,0,1),(4,3,2)] @?= Just [(1,2,3),(2,4,6)]
            computeSegment [(1,2,3),(7,6,5),(2,4,6),(3,0,1),(4,3,2)] @?= Just [(1,2,3),(7,6,5)]


    testComputeTriangle :: TestTree
    testComputeTriangle = HU.testCase "Compute triangles" $ do
            computeTriangle [] @?= Nothing
            computeTriangle [(1,2,3),(2,4,6),(7,6,5),(3,0,1),(4,3,2)] @?= Just [(1,2,3),(2,4,6),(7,6,5)]
            computeTriangle [(1,2,3),(2,4,6),(3,6,9),(7,6,5),(3,0,1),(4,3,2)] @?= Just [(1,2,3),(2,4,6),(7,6,5)]
        
    testComputeTetrahedron :: TestTree
    testComputeTetrahedron = HU.testCase "Compute tetrahedrons" $ do
            computeTetrahedron [] @?= Nothing
            computeTetrahedron [(1,2,3),(2,4,6),(7,6,5),(3,0,1),(4,3,2)] @?= Just [(1,2,3),(2,4,6),(7,6,5),(3,0,1)]
            computeTetrahedron [(1,2,3),(2,4,3),(3,6,3),(7,6,3),(3,0,1),(4,3,2)] @?= Just [(1,2,3),(2,4,3),(7,6,3),(3,0,1)]
        



    a,b,c,d,e,f :: Point3D
    a = (3,5,0)
    b = (0,6,0)
    c = (0,3,0)
    d = (2,2,0)
    e = (6,0,0)
    f = (9,3,0)

    testIsBetween3D :: TestTree
    testIsBetween3D = HU.testCase "IsBetween3D" $ do
        isBetween3D [a,c] b @?= False
        isBetween3D [f,b] a @?= True

    testsMergePoints :: TestTree
    testsMergePoints = HU.testCase "Merging points" $
        mergePoints [a,b,c,d] [a,d,e,f] @?= [b,c,e,f]
        

    list = [(6,0,10), (5,1,6), (4,2,0), (3,3,3), (2,4,1), (1,5,0), (0,6,9), (5,0,7), (4,1,2), (3,2,4), (2,3,0), (1,4,3), (0,5,8), (4,0,4), (3,1,0), (2,2,2), (1,3,1), (0,4,4), (3,0,3), (2,1,1), (1,2,1), (0,3,3), (2,0,1), (1,1,0), (0,2,1), (1,0,1), (0,1,1), (0,0,11)]
    list1 = take 23 list
    testsConvexHull3D :: TestTree
    testsConvexHull3D = HU.testCase "Compute convex hull 3D" $ do


        fmap fromConvexHull (convexHull3 [(1,1,2),(0,0,0),(3,3,3),(0,4,0),(4,0,0),(2,1,3),(2,2,2),(0,0,4),(4,4,0),(0,4,4),(4,0,4),(4,4,4)]) @?= Just (sort [(0,0,0),(0,4,0),(4,0,0),(0,0,4),(4,4,0),(0,4,4),(4,0,4),(4,4,4)])
        fmap fromConvexHull (convexHull3 [(1,1,1),(0,0,0),(3,3,3),(0,4,0),(4,0,0),(2,0,2),(2,2,2),(0,0,4),(4,4,0),(0,4,4),(4,0,4),(4,4,4)]) @?= Just (sort [(0,0,0),(0,4,0),(4,0,0),(0,0,4),(4,4,0),(0,4,4),(4,0,4),(4,4,4)])
        -- Disabled: expected output is the *lower* convex hull projected to
        -- z=1 (the operation used for regular subdivisions of Newton
        -- polytopes), not the full 3D convex hull that convexHull3 actually
        -- computes. (0,0,1) isn't even in list1. To re-enable, replace with
        -- the projected-subdivision operation or update the expectation to
        -- the true full hull of list1.
        -- fmap fromConvexHull (convexHull3 list1 ) @?= Just (sort [(0,0,1), (0,2,1), (2,0,1)])

    -- Full-dimensional hull: enumerate supporting planes through every input
    -- triple with integer cross products, retain planes with all points on one
    -- side, then retain points incident to three independent plane normals.
    -- This independent exact construction yields these 11 extreme points.
    testList1FullHull :: TestTree
    testList1FullHull = HU.testCase "3D hull of list1 preserves full extreme set" $
        fmap fromConvexHull (convexHull3 list1) @?= Just (sort
            [(0,3,3),(0,4,4),(0,5,8),(0,6,9),(1,2,1),(1,5,0)
            ,(2,0,1),(3,1,0),(4,0,4),(4,2,0),(6,0,10)])

    testTetrahedronHull :: TestTree
    testTetrahedronHull = HU.testCase "3D hull of tetrahedron" $
        fmap fromConvexHull (convexHull3 [(0,0,0), (0,2,0), (2,0,0), (1,1,1)]) @?= Just (sort [(0,0,0), (0,2,0), (2,0,0), (1,1,1)])

    testCubeHullInterior :: TestTree
    testCubeHullInterior = HU.testCase "3D cube hull excludes interior point" $
        fmap fromConvexHull (convexHull3 [(0,0,0),(3,3,3),(0,4,0),(4,0,0),(0,0,4),(4,4,0),(0,4,4),(4,0,4),(4,4,4)]) @?= Just (sort [(0,0,0),(0,4,0),(4,0,0),(0,0,4),(4,4,0),(0,4,4),(4,0,4),(4,4,4)])

    testFivePointHull :: TestTree
    testFivePointHull = HU.testCase "3D tetrahedron hull excludes fifth interior point" $
        fmap fromConvexHull (convexHull3 [(0,0,0),(4,0,0),(0,4,0),(0,0,4),(1,1,1)])
            @?= Just (sort [(0,0,0),(4,0,0),(0,4,0),(0,0,4)])

    -- Historical assertions flattened z to 1. These restored cases instead
    -- require the actual 3D extreme points of each coplanar input.
    testsCoplanarHull :: TestTree
    testsCoplanarHull = testGroup "Coplanar hull preserves coordinates"
        [ hullCase "historical z=0 lattice"
            [(3,0,0),(2,1,0),(1,2,0),(0,3,0),(2,0,0),(1,1,0),(0,2,0),(1,0,0),(0,1,0),(0,0,0)]
            [(3,0,0),(0,0,0),(0,3,0)]
        , hullCase "historical tilted triangle"
            [(3,0,1),(0,0,2),(0,3,1)] [(3,0,1),(0,0,2),(0,3,1)]
        , hullCase "historical three-point plane"
            [(1,2,3),(2,1,3),(5,3,1)] [(1,2,3),(2,1,3),(5,3,1)]
        , hullCase "historical triangle with edge midpoint"
            [(0,0,0),(0,2,0),(2,0,0),(1,1,0)] [(0,0,0),(0,2,0),(2,0,0)]
        , hullCase "vertical x-constant square"
            [(4,0,0),(4,2,0),(4,2,2),(4,0,2),(4,1,1)]
            [(4,0,0),(4,2,0),(4,2,2),(4,0,2)]
        , hullCase "vertical y-constant square"
            [(0,4,0),(2,4,0),(2,4,2),(0,4,2),(1,4,1)]
            [(0,4,0),(2,4,0),(2,4,2),(0,4,2)]
        , hullCase "tilted square with interior and edge points"
            [(0,0,3),(2,0,5),(2,2,9),(0,2,7),(1,1,6),(1,0,4)]
            [(0,0,3),(2,0,5),(2,2,9),(0,2,7)]
        , hullCase "vertical collinear endpoints"
            [(4,4,5),(4,4,1),(4,4,3),(4,4,1)] [(4,4,1),(4,4,5)]
        , hullCase "tilted collinear endpoints"
            [(1,2,3),(2,4,6),(3,6,9),(0,0,0)] [(0,0,0),(3,6,9)]
        , hullCase "single point" [(2,3,4)] [(2,3,4)]
        , hullCase "repeated single point" (replicate 4 (2,3,4)) [(2,3,4)]
        , HU.testCase "empty input has no hull" $
            fmap fromConvexHull (convexHull3 []) @?= Nothing
        ]
        where
            hullCase name points expected = HU.testCase name $ do
                fmap fromConvexHull (convexHull3 points) @?= Just (sort expected)
                fmap fromConvexHull (convexHull3 (reverse points)) @?= Just (sort expected)

    testsConvexHull3 :: TestTree
    testsConvexHull3 = testGroup "Test for convex hull in 3D" [testComputeSegment, testComputeTriangle, testComputeTetrahedron, testIsBetween3D, testsMergePoints, testsConvexHull3D, testTetrahedronHull, testCubeHullInterior, testList1FullHull, testFivePointHull, testsCoplanarHull]

