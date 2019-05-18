module TGeometry.TConvexHull (testsConvexHull) where

    import Test.Tasty
    import Test.Tasty.HUnit as HU
    import Geometry.ConvexHull
      
    
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
        

    
    testsConvexHull :: TestTree
    testsConvexHull = testGroup "Test for convex hull in 3D" [testComputeSegment, testComputeTriangle, testComputeTetrahedron]
    
    
    
    