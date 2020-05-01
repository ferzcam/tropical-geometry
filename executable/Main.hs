-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.

{-#LANGUAGE DataKinds #-}

module Main where

import Core
import Criterion.Main
import Data.Matrix
import qualified Data.Vector as V


verticess = [[0,0],[3,0],[0,3]]
uSet = toOrigin verticess

verticesssss = fromLists $ tail verticess

mUSet = fromLists (tail uSet)
vec = V.fromList ([1,1] :: [Rational])

b = [1,1,1] :: [Rational]
hyperplanes = facetEnumeration verticess


vertices2 = [[0,0,0],[0,0,1],[0,1,0],[1,0,0],[0,1,1],[1,0,1],[1,1,0],[1,1,1]] :: [Vertex]

main :: IO ()
main = putStrLn "Welcome to the tropical-geometry package"