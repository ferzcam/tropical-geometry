-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.

{-#LANGUAGE DataKinds #-}

module Hyper5 

(benchHyper5)

where 

import Core
import Criterion.Main

v, w, x, y, z :: Polynomial (Tropical Integer) Lex 5
v = variable 0
w = variable 1
x = variable 2
y = variable 3
z = variable 4


f1 = (x^2 + y^2 + (-1)*z^2)*(v^2+w^2+z^2 + (-1))




benchHyper5 :: Benchmark
benchHyper5 = bgroup "Hypersurfaces in 5 variables"
        [
            bench "f1" $ whnf verticesWithRays f1
        ]
    