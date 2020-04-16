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
        fmap fromConvexHull (convexHull3 [(3,0,0), (2,1,0), (1,2,0), (0,3,0), (2,0,0), (1,1,0), (0,2,0), (1,0,0), (0,1,0), (0,0,0)]) @?= Just ( sort [(3,0,1),(0,0,1),(0,3,1)])
        -- fmap fromConvexHull (convexHull3 [(3,0,1),(0,0,2),(0,3,1)]) @?= Just (sort [(3,0,1),(0,0,1),(0,3,1)])
        -- fmap fromConvexHull (convexHull3 [(1,2,3), (2,1,3), (5,3,1)]) @?= Just (sort [(1,2,1), (2,1,1), (5,3,1)])
        -- fmap fromConvexHull (convexHull3 [(0,0,0), (0,2,0), (2,0,0), (1,1,0)]) @?= Just (sort [(0,0,1), (0,2,1), (2,0,1)])
        -- fmap fromConvexHull (convexHull3 [(0,0,0), (0,2,0), (2,0,0), (1,1,1)]) @?= Just (sort [(0,0,0), (0,2,0), (2,0,0), (1,1,1)])
        -- fmap fromConvexHull (convexHull3 [(0,0,0),(3,3,3),(0,4,0),(4,0,0),(0,0,4),(4,4,0),(0,4,4),(4,0,4),(4,4,4)]) @?= Just (sort [(0,0,0),(0,4,0),(4,0,0),(0,0,4),(4,4,0),(0,4,4),(4,0,4),(4,4,4)])
        -- fmap fromConvexHull (convexHull3 [(1,1,2),(0,0,0),(3,3,3),(0,4,0),(4,0,0),(2,1,3),(2,2,2),(0,0,4),(4,4,0),(0,4,4),(4,0,4),(4,4,4)]) @?= Just (sort [(0,0,0),(0,4,0),(4,0,0),(0,0,4),(4,4,0),(0,4,4),(4,0,4),(4,4,4)])
        -- fmap fromConvexHull (convexHull3 [(1,1,1),(0,0,0),(3,3,3),(0,4,0),(4,0,0),(2,0,2),(2,2,2),(0,0,4),(4,4,0),(0,4,4),(4,0,4),(4,4,4)]) @?= Just (sort [(0,0,0),(0,4,0),(4,0,0),(0,0,4),(4,4,0),(0,4,4),(4,0,4),(4,4,4)])

    testsConvexHull3 :: TestTree
    testsConvexHull3 = testGroup "Test for convex hull in 3D" [testComputeSegment, testComputeTriangle, testComputeTetrahedron, testIsBetween3D, testsMergePoints, testsConvexHull3D]
        