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



mat = fromLists [
                        [-1,0,0],
                        [0,-1,0],
                        [0,0,-1],
                        [1,0,0],
                        [0,1,0],
                        [0,1,1],
                        [0,-1,1],
                        [1,0,1],
                        [-1,0,1]
                    ]

b = colFromList [0,0,0,1,1,2,1,2,1]

dictio = getDictionary mat b




x, y :: Polynomial (Tropical Integer) Lex 2
x = variable 0
y = variable 1

f1 = 1*x^2 + x*y + 1*y^2 + x + y + 2

points = expVecs f1

facetEnumerated = facetEnumeration $ extremalVertices points

matsHyp = fromLists $ map (\(_,h,_) -> h) facetEnumerated
bHyp = colFromList $ map (\(_,_,b) -> b) facetEnumerated



mats = map fromLists $ MS.elems $ normalCones points



rays = map (\mat -> let b = colFromList $ replicate (nrows mat) 0 in lrs mat b) mats

mat1 = head mats


lrs1 = lrs mat b
res1 = (sort [[0,0,0],[1,0,0],[1,1,0],[1,1,1],[1,0,1],[0,1,0],[0,1,1],[0,0,1],[1/2,1/2,3/2]]) == (sort.nub) lrs1

lrs2 = (sort.nub) $ lrs matsHyp bHyp

b1 :: Col
b1 =  colFromList $ replicate (nrows mat1) 0


cubeMat = fromLists [[0,1,0],[1,0,0],[0,0,1],[0,-1,0],[-1,0,0],[0,0,-1]] 

-- cubeMat = fromLists [[1,1,0],[1,0,1],[1,0,0],[1,-1,0],[1,0,0],[1,0,-1]] 

cubeB = colFromList [1,1,1,1,1,1]
lrsCube = (sort.nub) $ lrs cubeMat cubeB

main :: IO ()
main = putStrLn "Welcome to the tropical-geometry package"