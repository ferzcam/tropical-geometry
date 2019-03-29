-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import Test.Tasty

import TPolynomial.TMonomial
import TPolynomial.TPrelude
import TPolynomial.TNumeric
import TArithmetic.TNumbers

main :: IO ()
main = do
    defaultMain allTests



allTests ::   TestTree
allTests = testGroup "Tasty tests" [

        testGroup "Monomial" [testsNumbers ,testsMonomial, testsPrelude, testsNumeric]
    ]

