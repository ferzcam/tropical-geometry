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
      


    testSumMatrix :: TestTree
    testSumMatrix =   HU.testCase "Sum of matrices" $ do
            a+b  @?= aPb
            c + transp d @?= cPd'

    testProdMatrix :: TestTree
    testProdMatrix =   HU.testCase "Product of matrices" $ do
            a*b  @?= aTb
            c*a  @?= cTa
            c*d  @?= cTd
            d*c  @?= dTc

    testsMatrices :: TestTree
    testsMatrices = testGroup "Test for tropical matrices" [testSumMatrix, testProdMatrix]
    
    
    
    