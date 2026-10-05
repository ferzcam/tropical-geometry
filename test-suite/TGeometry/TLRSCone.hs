module TGeometry.TLRSCone (testsLRSCone) where

import Data.List (nub, sort)
import Data.Matrix (fromLists)
import Data.Ratio ((%))
import Geometry.LRS (colFromList, lrs)
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

type Point = [Rational]

-- Homogeneous Ax <= 0 representation of the original four-ray cone.
coneRows :: [Point]
coneRows = [[-1,0,2],[-1,1,0],[0,1,0],[1,0,1]]

expectedDirections :: [Point]
expectedDirections = [[-2,-2,-1],[0,-1,0],[0,0,-1],[1,0,-1]]

-- Rays are equivalent only under positive scaling. Dividing by the absolute
-- first nonzero coordinate keeps opposite rays distinct.
normalize :: Point -> Point
normalize point = case dropWhile (== 0) point of
    [] -> error "normalize: zero is not a ray"
    first:_ -> map (/ abs first) point

checkCone :: String -> [Point] -> TestTree
checkCone name rows = testCase name $ do
    let rays = lrs (fromLists rows) (colFromList (replicate (length rows) 0)) [0,0,0]
    assertBool "Every returned ray is nonzero" (all (any (/= 0)) rays)
    assertBool "Every returned ray has the ambient dimension" (all ((== 3) . length) rays)
    assertBool "Every returned ray satisfies A*r <= 0"
        (all (\ray -> all (\row -> sum (zipWith (*) row ray) <= 0) rows) rays)
    assertEqual "Exact set of oriented ray directions"
        (sort (nub (map normalize expectedDirections)))
        (sort (nub (map normalize rays)))

testsLRSCone :: TestTree
testsLRSCone = localOption (mkTimeout 30000000) $
    testGroup "LRS cone direction invariance"
        [ checkCone "Original cone" coneRows
        , checkCone "Reversed constraint order" (reverse coneRows)
        , checkCone "Positive rational row scaling"
            (zipWith (\factor -> map (* factor)) [1%2,3,2%3,7%4] coneRows)
        ]
