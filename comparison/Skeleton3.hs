-- Comparison-only compatibility exports and hand-known self-test fixtures.
module Skeleton3 (module Geometry.TropicalSkeleton3, skeleton3Checks) where

import Geometry.TropicalSkeleton3
import Geometry.TropicalSlice (Term3(..))

-- | Hand-known exact checks retained for the comparison executable's
-- @--self-test@ option. These fixtures are not part of the library API.
skeleton3Checks :: [(String,Bool)]
skeleton3Checks =
    [ ("min(0,x,y,z) has one vertex and four rays", exactSkeleton3 simplex == Right (fan [0,0,0]))
    , ("fractionally shifted simplex", exactSkeleton3 shifted == Right (fan [1/2,2/3,3/4]))
    , ("quadratic simplex with redundant support", exactSkeleton3 quadratic == Right (fan [0,0,0]))
    , ("quadratic with a bounded skeleton edge", exactSkeleton3 splitQuadratic == Right splitExpected)
    , ("permutation invariance", exactSkeleton3 (reverse shifted) == exactSkeleton3 shifted)
    , ("duplicate exponent retains minimum", exactSkeleton3 (Term3 1 0 0 10:simplex) == exactSkeleton3 simplex)
    , ("affine exponent support rejected", case exactSkeleton3 (take 3 simplex) of Left _ -> True; Right _ -> False)
    , ("canonical full line anchor", canonicalSkeleton3 [Line3 [2,3,4] [-2,-2,0]] == canonicalSkeleton3 [Line3 [0,1,4] [1,1,0]])
    ]
  where
    simplex = [Term3 0 0 0 0,Term3 1 0 0 0,Term3 0 1 0 0,Term3 0 0 1 0]
    shifted = [Term3 0 0 0 0,Term3 1 0 0 (-1/2),Term3 0 1 0 (-2/3),Term3 0 0 1 (-3/4)]
    quadratic = [Term3 x y z 0 | x <- [0..2],y <- [0..2],z <- [0..2],x+y+z <= 2]
    splitQuadratic = [Term3 0 0 0 0,Term3 1 0 0 (-1),Term3 2 0 0 0,Term3 0 1 0 0,Term3 0 0 1 0]
    splitExpected = canonicalSkeleton3
        ([Segment3 [-1,-2,-2] [1,0,0]] ++
         [Ray3 [1,0,0] d | d <- [[1,0,0],[0,1,0],[0,0,1]]] ++
         [Ray3 [-1,-2,-2] d | d <- [[0,1,0],[0,0,1],[-1,-2,-2]]])
    fan point = canonicalSkeleton3 [Ray3 point direction | direction <- [[1,0,0],[0,1,0],[0,0,1],[-1,-1,-1]]]
