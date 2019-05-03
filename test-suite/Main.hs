-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import Test.Tasty

import TPolynomial.TMonomial
import TPolynomial.TMonomial1
import TPolynomial.TPrelude
import TArithmetic.TNumbers
import TArithmetic.TMatrix

main :: IO ()
main = do
    defaultMain allTests



allTests ::   TestTree
allTests = testGroup "Tasty tests" [

        testGroup "List of tests:" [testsNumbers, testsMatrices, testsMonomial, testsMonomial1, testsPrelude]
    ]

