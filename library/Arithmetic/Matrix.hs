module Arithmetic.Matrix where

import Arithmetic.Numbers
import Numeric.Algebra hiding ((+), (*))
import Data.List
-- import Data.Matrix hiding (transpose, zero)


matrixList = [[Tropical 0, Tropical 3, Inf, Inf, Inf], 
    [Tropical 4, Tropical 0, Tropical 1, Inf, Tropical 8],
    [Inf, Inf, Tropical 0, Tropical 1, Inf],
    [Inf, Inf, Inf, Tropical 0, Tropical 1],
    [Inf, Inf, Inf, Inf, Tropical 0]
    ]


matrixList1 = [[Tropical 0, Tropical 2, Inf, Tropical 3], 
    [Inf, Tropical 0, Inf, Tropical 4],
    [Tropical 8, Inf, Tropical 0, Tropical 1],
    [Tropical 1, Inf, Inf, Tropical 0]
    ]


matrixList2 = [[Tropical 0, Tropical 1, Tropical 3, Tropical 7],
     [Tropical 2, Tropical 0, Tropical 1, Tropical 3],
     [Tropical 4, Tropical 5, Tropical 0, Tropical 1],
     [Tropical 6, Tropical 3, Tropical 1, Tropical 0]
  ]

vector = [Tropical 0, Tropical 1, Tropical 3, Tropical 7]

prodVectorMatrix :: (Monoidal a, Num a) => [a] -> [[a]] -> [a]
prodVectorMatrix v m = map (foldl' (+) zero ) (foo v m)
    where
    foo l mat
        | (length $ mat!!0) == 0 = []
        | otherwise = (zipWith (*) l (map head mat)):(foo l (map tail mat))

prodMatMat :: (Monoidal a, Num a) => [[a]] -> [[a]] -> [[a]]
prodMatMat [] _ = []
prodMatMat (x:xs) mat2 = (prodVectorMatrix x mat2) : (prodMatMat xs mat2)


--tropMatrix = fromLists matrixList

mmult :: (Num a, Monoid a) => [[a]] -> [[a]] -> [[a]] 
mmult a b = [[ foldr (+) 0 $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a ]
