module Polynomial.Polytope where

import Polynomial.Prelude
import Polynomial.Monomial
import Data.List
import qualified Data.Map.Strict as MS
import qualified Data.Sized.Builtin as DS
-- | Funcionts first, second and third are auxiliary functions for permuting the positions of the columns of a matrix to find the determinant

first :: [a] -> a
first [] = error "Empty list"
first (x:y) = x

second :: [a] -> a
second [] = error "Empty list"
second [a] = error "List with just one element"
second (x:y:z) = y

third :: [a] -> a
third [] = error "Empty list"
third [a] = error "List with just one element"
third [a, b] = error "List with just two elements"
third (x:y:z:w) = z

positions :: [[a] -> a]
positions = [first, second, third]

permutePositions :: [[[a] -> a]]
permutePositions = permutations positions


-- | Given a list [a,b,c,d,e,f], the resulting list is [a,b,d,c,e,f]
fliptListAtCenter :: [a] -> [a]
fliptListAtCenter lst = firstPart ++ secondPart
    where
        splitted = splitAt (div (length lst) 2) lst
        firstPart = (++) <$> (init . fst) <*> (return . head . snd) $ splitted
        secondPart = (++) <$> (return . last . fst) <*> (tail . snd) $ splitted

determinant :: Num a => [[a]] -> a
determinant lst = computeProds 1 $ fliptListAtCenter $ zipWith (zipWith ($)) permutePositions (repeat lst)

computeProds :: Num a => a -> [[a]] -> a
computeProds _ [] = 0
computeProds num (x:xs) = num * (foldl1 (*) x) + computeProds (-num) xs

{-|

The process is the following:
Let a = [[1,2,3], [4,5,6], [7,8,9]]

We take the permutations needed for calculating the determinant, those are:

[[1,5,9],[2,4,9],[2,6,7],[3,5,7],[3,4,8],[1,6,8]]

After that multiply every sublist:

[45,72,84,105,96,48]

Finally, we do: 45 - 72 + 84 - 105 + 96 - 48 = 0

In this case, the determinant is 0.

-}

points :: (Fractional a) => [[a]]
points = [[0,1],[1,3],[2,1],[3,4],[4,0],[5,2]]

complete :: Num a => [[a]] -> [[a]]
complete = map (\[a,b] -> [a,b,1])

dropExtraOne :: (Eq a, Num a) => [[a]] -> [[a]]
dropExtraOne = map (\[a,b,1] -> [a,b])

semiHullUp :: (Ord a, Num a) => [[a]] -> [[a]]
semiHullUp [a] = [a]
semiHullUp [a,b] = [a,b]
semiHullUp (x:y:z:w) = case (determinant [y,x,z]) > 0 of
                    True -> x:(semiHullUp (y:z:w))
                    False -> semiHullUp (x:z:w)

semiHullDown :: (Ord a, Num a) => [[a]] -> [[a]]
semiHullDown [a] = [a]
semiHullDown [a,b] = [a,b]
semiHullDown (x:y:z:w) = case (determinant [y,x,z]) < 0 of
                    True -> x:(semiHullDown (y:z:w))
                    False -> semiHullDown (x:z:w)
                    


leftMost :: (Ord a) => [[a]] -> [a]
leftMost lst = foldl1 (\l1@(a:b) l2@(c:d) -> if c < a then l2 else l1) lst

rightMost :: (Ord a) => [[a]] -> [a]
rightMost lst = foldl1 (\l1@(a:b) l2@(c:d) -> if c > a then l2 else l1) lst

isPointUp :: (Ord a, Fractional a) => [a] -> [a] -> [a] -> Bool
isPointUp (l1:l2:x) (r1:r2:y) (p1:p2:z) = p2 >= (m*p1 + b)
    where
        m = (r2-l2)/(r1-l1)
        b = l2 - m*l1

isPointDown :: (Ord a, Fractional a) => [a] -> [a] -> [a] -> Bool
isPointDown (l1:l2:x) (r1:r2:y) (p1:p2:z) = p2 <= (m*p1 + b)
    where
        m = (r2-l2)/(r1-l1)
        b = l2 - m*l1
        

convexHull :: (Ord a, Eq a, Fractional a) => [[a]] -> [[a]]
convexHull lst = dropExtraOne $ union lowerHull upperHull
        where 
            lstC = complete lst
            left = leftMost lstC
            right = rightMost lstC
            lowerHull = semiHullDown $ filter (\a -> isPointDown left right a) lstC
            upperHull = semiHullUp $ filter (\a -> isPointUp left right a) lstC


polytope :: (Ord a, Fractional a) => IsOrderedPolynomial poly => poly -> [[a]]
polytope = convexHull . (map (map fromIntegral)) . sort . (map (DS.toList . getMonomial . fst)) . (MS.toList) . terms
    -- | sNatToInt (arity poly) /= 2 = error "Algorithm works only for polynomials in 2 variables"
    -- | otherwise = (convexHull . sort . (map (DS.toList . getMonomial . fst)) . (MS.toList) . terms) poly