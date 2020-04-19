{-# LANGUAGE FlexibleInstances #-}

module Util where

import Data.List (nub, sort)



combinations :: (Eq a, Ord a) => [a] -> Int -> [[a]]
combinations list k = nub $ map sort $ filter ((k==).length.nub) $ mapM (const list) [1..k]

safeZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
safeZipWith f l1 l2
    | length l1 /= length l2 = error "Lists must have the same size for safe zipping"
    | otherwise = zipWith f l1 l2

dot :: (Num a) => [a] -> [a] -> a
dot l1 = (foldr1 (+)) . (safeZipWith (*) l1)

($-$) :: (Num a) => [a] -> [a] -> [a]
($-$) = safeZipWith (-)

($+$) :: (Num a) => [a] -> [a] -> [a]
($+$) = safeZipWith (+)



instance Num [Rational] where 
    (+) = ($+$) 
    --(*)  
--    abs = map abs
--    signum = map signum, fromInteger, (negate | (-))

instance Num Bool
