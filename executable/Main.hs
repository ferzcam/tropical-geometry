-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.

{-#LANGUAGE DataKinds, NoImplicitPrelude #-}

module Main where

import Prelude hiding ((+),(*),(^), recip)

import Core
import Criterion.Main
import Data.Matrix
import qualified Data.Map.Strict as MS
import Data.List
import qualified Data.Vector as V
import Numeric.Algebra hiding ((/))
import Polynomial.Variety

----------

-- -- x, y, z:: Polynomial (Tropical Integer) Lex 3
-- -- x = variable 0
-- -- y = variable 1
-- -- z = variable 2

-- -- f2 =  3*x*y*z + x + y + 2*z + (-2)


-- v, w, x, y, z :: Polynomial (Tropical Integer) Lex 5
-- v = variable 0
-- w = variable 1
-- x = variable 2
-- y = variable 3
-- z = variable 4


-- f1 = (x^2 + y^2 + (-1)*z^2)*(v^2+w^2+z^2 + (-1))

x, y :: Polynomial (Tropical Integer) Lex 2
x = variable 0
y = variable 1

f1 = 1*x^2 + x*y + 1*y^2 + x + y + 2
f2 = 3*x^2 + x*y + 3*y^2 + 1*x + 1*y + 0
f3 = 3*x^3 + 1*x^2*y + 1*x*y^2 + 3*y^3 + 1*x^2 + x*y + 1*y^2 + 1*x + 1*y + 3
f4 = x^3 + x^2*y + x*y^2 + y^3 + x^2 + x*y + y^2 + x + y + 0


main :: IO ()
main = putStrLn "Welcome to the tropical-geometry package"