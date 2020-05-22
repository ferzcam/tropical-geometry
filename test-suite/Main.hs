-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.

--{-# OPTIONS_GHC -F -pgmF hlint-test #-}

import Test.Tasty

import TPolynomial.TMonomial
import TPolynomial.TPrelude
import TArithmetic.TNumbers
import TArithmetic.TMatrix
import TGeometry.TVertex
import TGeometry.TFacet
import TGeometry.TPolyhedral
import TPolynomial.THypersurface
import TPolynomial.THypersurface3
import TPolynomial.THypersurface4
import TPolynomial.THypersurface5
import TGeometry.TLRSPol2

main :: IO ()
main = do
  print f10
  defaultMain allTests

                --testsNumbers, 
            --testsMatrices, 
            --testsMonomial, 
            --testsPrelude, 
            --testsPolyhedral,
       --     testsHypersurface3,
         --   testsHypersurface4,
          --  testsSimplex]
           -- testsFacet]

allTests ::   TestTree
allTests = testGroup "Tasty tests" [
        testGroup "List of tests:" [
            testsNumbers, 
            testsMatrices, 
            testsMonomial, 
            testsPrelude, 
            testsPolyhedral, 
            testsHypersurface,
            testsHypersurface3,
            testsHypersurface4,
            testsHypersurface5,
            testsSimplex,
            testsVertexEnumPol2]
           -- testsFacet]
    ]
