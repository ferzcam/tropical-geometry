module TArithmetic.TMatrix (testsMatrices) where

    import Test.Tasty
    import Test.Tasty.HUnit as HU
    import Arithmetic.Numbers
    import Arithmetic.Matrix
    import Data.List (transpose)
        

    a = TMatrix
        [
        [1,     2,      3],
        [2,     Inf,    5],
        [4,     8,      -2]
        ] :: TMatrix (Tropical Integer)
    
    b = TMatrix
        [
        [6,     Inf,   -1],
        [0,      -9,    8],
        [Inf,     2,    0]
        ] :: TMatrix (Tropical Integer)

    c = TMatrix
        [
        [0,       6,    2],
        [1,       0,    1]
        ] :: TMatrix (Tropical Integer)
  
    d = TMatrix
        [
        [4,       1],
        [2,       2],
        [0,     Inf]
        ] :: TMatrix (Tropical Integer)

-- | Results

    aPb = TMatrix
          [
          [1,     2,    -1],
          [0,    -9,    5],
          [4,     2,   -2]
          ] :: TMatrix (Tropical Integer)
    
    cPd' = TMatrix
           [
           [0,       2,    0],
           [1,       0,    1]
           ] :: TMatrix (Tropical Integer)

    aTb = TMatrix
          [
          [2,      -7,    0],
          [8,       7,    1],
          [8,      -1,   -2]
          ] :: TMatrix (Tropical Integer)
  
    cTa = TMatrix
          [
          [1,   2,   0],
          [2,   3,  -1]
          ] :: TMatrix (Tropical Integer)

    cTd = TMatrix
          [
          [2,   1],
          [1,   2]
          ] :: TMatrix (Tropical Integer)
      
    dTc = TMatrix
          [
          [2,      1,    2],
          [2,      2,    3],
          [0,      6,    2]
          ] :: TMatrix (Tropical Integer)
      


    testSumMatrix1 :: TestTree
    testSumMatrix1 =   HU.testCase "a+b" $ do
            a+b  @?= aPb

    testSumMatrix2 :: TestTree
    testSumMatrix2 =   HU.testCase "c+d'" $ do
            c + (transp d) @?= cPd'

    testProdMatrix1 :: TestTree
    testProdMatrix1 =   HU.testCase "ab" $ do
            a*b  @?= aTb

    testProdMatrix2 :: TestTree
    testProdMatrix2 =   HU.testCase "ca" $ do
            c*a  @?= cTa
    
    testProdMatrix3 :: TestTree
    testProdMatrix3 =   HU.testCase "cd" $ do
            c*d  @?= cTd
        
    testProdMatrix4 :: TestTree
    testProdMatrix4 =   HU.testCase "dc" $ do
            d*c  @?= dTc
        
    testSumMatrix :: TestTree
    testSumMatrix = testGroup "Test for summing tropical matrices" [testSumMatrix1, testSumMatrix2]

    testProdMatrix :: TestTree
    testProdMatrix = testGroup "Test for multiplying tropical matrices" [testProdMatrix1, testProdMatrix2, testProdMatrix3, testProdMatrix4]


    testsMatrices :: TestTree
    testsMatrices = testGroup "Test for tropical matrices" [testSumMatrix, testProdMatrix]
    
    
    
    