-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.

{-#LANGUAGE DataKinds #-}

import Core
import Criterion.Main


x, y :: Polynomial (Tropical Integer) Lex 2
x = variable 0
y = variable 1

f1,f2,f3,f4,f5 :: Polynomial (Tropical Integer) Lex 2

f1 = 1*x^2 + x*y + 1*y^2 + x + y + 2
f2 = 3*x^2 + x*y + 3*y^2 + 1*x + 1*y + 0
f3 = 3*x^3 + 1*x^2*y + 1*x*y^2 + 3*y^3 + 1*x^2 + x*y + 1*y^2 + 1*x + 1*y + 3
f4 = x^3 + x^2*y + x*y^2 + y^3 + x^2 + x*y + y^2 + x + y + 0
f5 = 2*x*y^^(-1) + 2*y^^(-1) + (-2)

main :: IO ()
main = defaultMain [
        bgroup "Vertices and normals"
        [
            bench "f1" $ whnf verticesNormals f1
        ,   bench "f2" $ whnf verticesNormals f2
        ,   bench "f3" $ whnf verticesNormals f3
        ,   bench "f4" $ whnf verticesNormals f4
        ]
    ] 