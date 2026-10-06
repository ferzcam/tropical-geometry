-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.

--{-# OPTIONS_GHC -F -pgmF hlint-test #-}

import Test.Tasty

import TPolynomial.TMonomial
import TPolynomial.TPrelude
import TArithmetic.TNumbers
import TArithmetic.TMatrix
import TGeometry.TConvexHull2
import TGeometry.TConvexHull3
import TGeometry.TPolyhedral
import TGeometry.TPolytope
import TGeometry.TLRSPol2
import TGeometry.TLRSRegression
import TGeometry.TLRSCyclic
import TGeometry.TLRSCone
import TGeometry.TLRSDegenerate
import TGeometry.TTropicalCurve
import TGeometry.TLRSInputValidation
import TPolynomial.THypersurface

main :: IO ()
main = defaultMain allTests



allTests ::   TestTree
allTests = testGroup "Tasty tests" [
        testGroup "List of tests:" [
            testsNumbers, 
            testsMatrices, 
            testsMonomial, 
            testsPrelude, 
            testsConvexHull2, 
            testsConvexHull3, 
            testsPolyhedral,
            testsPolytope,
            testsVertexEnumPol2,
            testsLRSRegression,
            testsLRSCyclic,
            testsLRSCone,
            testsLRSDegenerate,
            testsLRSInputValidation,
            testsTropicalCurve,
            testsHypersurface]
    ]
