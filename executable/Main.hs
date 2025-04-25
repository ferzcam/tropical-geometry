-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.

{-#LANGUAGE DataKinds #-}

module Main where

import Core
import Criterion.Main


x, y :: Polynomial (Tropical Integer) Lex 2
x = variable 0
y = variable 1

f1,f2,f3,f4,f5,f6,f7,f8 :: Polynomial (Tropical Integer) Lex 2

f1 = 1*x^2 + x*y + 1*y^2 + x + y + 2
f2 = 3*x^2 + x*y + 3*y^2 + 1*x + 1*y + 0
f3 = 3*x^3 + 1*x^2*y + 1*x*y^2 + 3*y^3 + 1*x^2 + x*y + 1*y^2 + 1*x + 1*y + 3
f4 = x^3 + x^2*y + x*y^2 + y^3 + x^2 + x*y + y^2 + x + y + 0
f5 = 2*x*y^^(-1) + 2*y^^(-1) + (-2)
f6 = 6*x^4 + 4*x^3*y + 3*x^2*y^2 + 4*x*y^3 + 5*y^4 + 2*x^3 + x^2*y + 1*x*y^2 + 4*y^3 + 2*x^2 + x*y + 3*y^2 + x + 2*y + 5
f7 = 6*x^5 + 2*x^4*y + 4*x^3*y^2 + x^2*y^3 + 3*x*y^4 + 8*y^5 + 6*x^4 + 4*x^3*y + 3*x^2*y^2 + 4*x*y^3 + 5*y^4 + 2*x^3 + x^2*y + 1*x*y^2 + 4*y^3 + 2*x^2 + x*y + 3*y^2 + x + 2*y + 5
f8 = 10*x^6 + 8*x^5*y + 6*x^4*y^2 + 6*x^3*y^3 + 4*x^2*y^4 + 6*x*y^5 + 9*y^6 + 6*x^5 + 2*x^4*y + 4*x^3*y^2 + x^2*y^3 + 3*x*y^4 + 8*y^5 + 6*x^4 + 4*x^3*y + 3*x^2*y^2 + 4*x*y^3 + 5*y^4 + 2*x^3 + x^2*y + 1*x*y^2 + 4*y^3 + 2*x^2 + x*y + 3*y^2 + x + 2*y + 10


main :: IO ()
main = putStrLn "Hi"
  -- defaultMain [
        -- bgroup "Vertices and normals"
        -- [
            -- bench "f1" $ whnf verticesNormals f1
        -- ,   bench "f2" $ whnf verticesNormals f2
        -- ,   bench "f3" $ whnf verticesNormals f3
        -- ,   bench "f4" $ whnf verticesNormals f4
        -- ,   bench "f5" $ whnf verticesNormals f5
        -- ,   bench "f6" $ whnf verticesNormals f6
        -- ,   bench "f7" $ whnf verticesNormals f7
        -- ,   bench "f8" $ whnf verticesNormals f8
        -- ]
    -- ] 

-- main :: IO ()
-- main = putStrLn "Welcome to the tropical-geometry package"
