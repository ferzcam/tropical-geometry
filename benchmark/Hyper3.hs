-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.

{-#LANGUAGE DataKinds #-}

module Hyper3

(benchHyper3)

where 

import Core
import Criterion.Main


x, y, z :: Polynomial (Tropical Integer) Lex 3
x = variable 0
y = variable 1
z = variable 2

f1 = 3*x*y*z + x + y + 2*z + (-2)
f2 = x + y + z + 1 
f3 = 1*y^2 + (-1)*x^2 + (-1)*z^2 + (-1)*x^3



benchHyper3 :: Benchmark
benchHyper3 = bgroup "Hypersurfaces in 3 variables"
        [
            bench "f1" $ whnf verticesWithRays f1
        ,   bench "f2" $ whnf verticesWithRays f2
        ,   bench "f3" $ whnf verticesWithRays f3
        ]
    