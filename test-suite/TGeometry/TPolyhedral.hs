module TGeometry.TPolyhedral (testsPolyhedral) where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Geometry.Polyhedral

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as MS


set1 = [[5,1], [6,3], [6,5], [1,5], [2,2], [5,2], [3,4]]
set2 = [[1,0,0], [0,1,0], [0,0,1], [1,1,1], [0.5, 0.5, 0.5]]

testNormalCones :: TestTree
testNormalCones = HU.testCase "Test for vertices and their normals" $ do
        normalCones set1 @?= MS.empty
        normalCones set2 @?= MS.empty

testsPolyhedral :: TestTree
testsPolyhedral = testGroup "Test for computing polyhedral algorithms" []
