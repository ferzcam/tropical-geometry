-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.

{-#LANGUAGE DataKinds #-}

module Hyper4 

(benchHyper4)

where 

import Core
import Criterion.Main


w, x, y, z :: Polynomial (Tropical Integer) Lex 4
w = variable 0
x = variable 1
y = variable 2
z = variable 3


f1 = 3*x*w + 2*x^2*w^2 + (-5)*x*y + z + 10*x^2*y*w + y*z + (-3)



benchHyper4 :: Benchmark
benchHyper4 = bgroup "Hypersurfaces in 4 variables"
        [
            bench "f1" $ whnf verticesWithRays f1
        ]
    