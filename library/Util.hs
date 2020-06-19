{-# LANGUAGE FlexibleInstances #-}

module Util where

import Data.List (nub, sort, findIndex)
import Data.Matrix hiding (trace)
import qualified Data.Vector as V
import Data.Traversable
import Debug.Trace
import Control.Arrow
import Data.Maybe

combinations :: (Eq a, Ord a) => [a] -> Int -> [[a]]
combinations list k = nub $ map sort $ filter ((k==).length.nub) $ mapM (const list) [1..k]

safeZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
safeZipWith f l1 l2
    | length l1 /= length l2 = error "Lists must have the same size for safe zipping"
    | otherwise = zipWith f l1 l2

safeZipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
safeZipWith3 f l1 l2 l3
    | length l1 /= length l2 || length l1 /= length l3 = error "Lists must have the same size for safe zipping"
    | otherwise = zipWith3 f l1 l2 l3



dot :: (Num a) => [a] -> [a] -> a
dot l1 = (foldr1 (+)) . (safeZipWith (*) l1)

($-$) :: (Num a) => [a] -> [a] -> [a]
($-$) = safeZipWith (-)

($+$) :: (Num a) => [a] -> [a] -> [a]
($+$) = safeZipWith (+)


safeZipWithV :: (a -> b -> c) -> V.Vector a -> V.Vector b -> V.Vector c
safeZipWithV f l1 l2
    | V.length l1 /= V.length l2 = error "Vectors must have the same size for safe zipping"
    | otherwise = V.zipWith f l1 l2

dotV :: (Num a) => V.Vector a -> V.Vector a -> a
dotV v1 = (foldr (+) 0) . (safeZipWithV (*) v1)


prodMatVec :: (Num a) => Matrix a -> V.Vector a -> V.Vector a
prodMatVec matrix vector
    | ncols matrix /= length vector = error "Dimensions for matrix product with a vector do not match"
    | otherwise = V.fromList $ map (dot (V.toList vector)) (toLists matrix)


-- | Solves a linear equality system @A x = b@ given by an upper triangular matrix via
-- backward substitution.
backwardSub :: (Fractional a) => Matrix a -> V.Vector a -> V.Vector a
backwardSub upper bV =
  backwardSub' upper bV V.empty (nrows upper)
  where
    backwardSub' upper bV xV i 
      | nrows upper == 0 = xV
      | i==0             = xV
      | otherwise = 
        let curRow = snd $ V.splitAt i $ getRow i upper 
            lm   =  (getRow i upper) V.! (i-1)
            curB  = bV V.! (i-1)
            negSum = xV `dotV` curRow
            curX = (curB - negSum) / lm
        in
        backwardSub' upper bV (curX `V.cons` xV) (i-1)


forwardSub :: (Fractional a) => Matrix a -> V.Vector a -> V.Vector a
forwardSub lower bV = reverseV $ backwardSub (reverseM lower) (reverseV bV)
    where
        reverseM = fromLists . reverse. (map reverse) . toLists
        reverseV = V.fromList . reverse .V.toList

solveLS :: (Fractional a, Ord a, Show a) => Matrix a -> V.Vector a -> Maybe (V.Vector a)
solveLS matrix vector = case luDecomp matrix of 
                            Nothing -> Nothing
                            Just (u,l,perm,_) ->  let y = forwardSub l (prodMatVec perm vector)
                                                in Just $ backwardSub u y
   

--instance Num [Rational]
      


instance Num Bool where
    fromInteger 0 = False
    fromInteger 1 = True




sndThrd :: (a,b,c) -> (b,c)
sndThrd (_,b,c) = (b,c)


lcmList :: [Integer] -> Integer
lcmList [] = error "lcmList: list must have at least one element"
lcmList [x] = x
lcmList (x:y:z) = lcmList ((lcm x y):z)

gcdList :: [Integer] -> Integer
gcdList [] = error "lcmList: list must have at least one element"
gcdList [x] = x
gcdList (x:y:z) = gcdList ((gcd x y):z)


{- |

Given two points p1 and p2, the parameterized line between them is of the form 
    x_i = p1_i + (p2_i - p1_i)t
where t is the parameter.

We return a cured version of the parameterization:
    t = (x_i - p1_i) / (p2_i - p1_i)
 -}
 
-- line :: (Fractional a) => [a] -> [a] -> [a->a]
-- line p1 p2
--     | length p1 /= length p2 = error "line: Points must have de the same dimension in order to construct the line between them."
--     | otherwise = safeZipWith parameterize p1 p2
--     where
--         parameterize ai bi = \x -> (x-ai)/(bi-ai) 


{- |

Given a points p and a direction vector v, the parameterized line is of the form 
    x_i = p_i + v_i * t
where t is the parameter.

 -}
 
line :: [Rational] -> [Integer] -> [Rational-> Rational]
line p v
    | length p /= length v = error "line: Points must have de the same dimension in order to construct the line between them."
    | otherwise = safeZipWith parameterize p (map toRational v)
    where
        parameterize pi vi = \t -> pi + vi*t

{- 
    If the direction primitive vector contains a zero in the ith position, 
    that means that the line does not depend on the ith variable. We remove that for the computation
    to avoid a exception caused by division by zero.
 -}


getParameter ::
        [Rational]  -- | Point from which the ray emanates
    ->  [Integer]  -- | Emanating vector
    ->  [Rational]  -- | Point to evaluate
    ->  Rational
getParameter p v x
    | length p /= length v = error "getParameter: Points must have de the same dimension in order to construct the line between them."
    | idx == Nothing = error "getParameter: primitive vector must not be 0" 
    | otherwise = let i = fromJust idx in (x!!i - p!!i)/(toRational (v!!i))
    where
        idx = findIndex (/=0) v


removeZeros :: (Fractional a, Eq a) => [a] -> [a] -> ([a],[a])
removeZeros [] [] = error "removeZeros: Point representation cannot be an empty list."
removeZeros [x] [y] = if y == 0 then ([],[]) else ([x],[y]) 
removeZeros p@(x:xs) v@(y:ys) 
    | length p /= length v = error "removeZeros: Points must have the same dimension."
    | y == 0 = removeZeros xs ys
    | y /= 0 = ((x:) *** (y:)) (removeZeros xs ys) 