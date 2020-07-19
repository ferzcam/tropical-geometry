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

x, y, z:: Polynomial (Tropical Integer) Lex 3
x = variable 0
y = variable 1
z = variable 2

f2 =  3*x*y*z + x + y + 2*z + (-2)


main :: IO ()
main = putStrLn "Welcome to the tropical-geometry package"