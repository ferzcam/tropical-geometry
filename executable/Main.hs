-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.

{-#LANGUAGE DataKinds #-}

module Main where

import Core
import Criterion.Main
import Data.Matrix
import qualified Data.Map.Strict as MS
import Data.List
import qualified Data.Vector as V


x, y :: Polynomial (Tropical Integer) Lex 2
x = variable 0
y = variable 1

f1 = 2*x^2 + 2*x*y + 4*y^2 + 2*x + 1*y + 2
f2 = 3*x^2 + 1*x*y + 2*y^3 + 1*x + 2*y + 3 


main :: IO ()
main = putStrLn "Welcome to the tropical-geometry package"