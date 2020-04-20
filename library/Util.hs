{-# LANGUAGE FlexibleInstances #-}

module Util where

import Data.List (nub, sort)
import Data.Matrix hiding (trace)
import qualified Data.Vector as V
import Data.Traversable
import Debug.Trace

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


safeZipWithV :: (a -> b -> c) -> V.Vector a -> V.Vector b -> V.Vector c
safeZipWithV f l1 l2
    | V.length l1 /= V.length l2 = error "Vectors must have the same size for safe zipping"
    | otherwise = V.zipWith f l1 l2

dotV :: (Num a) => V.Vector a -> V.Vector a -> a
dotV v1 = (foldr (+) 0) . (safeZipWithV (*) v1)

-- | Solves a linear equality system @A x = b@ given by a lower triangular matrix via
-- forward substitution.
-- forwardSub :: Fractional a => Matrix a -> V.Vector a -> V.Vector a
-- forwardSub = forwardSub' (V.empty) 
--   where
--     forwardSub' xV lower bV 
--         | nrows lower == 0 = xV
--         | nrows lower == 1 = (V.snoc xV curX)
--         | otherwise = forwardSub' (V.snoc xV curX)
--                         (submatrix 2 (nrows lower) 1 (ncols lower) lower) 
--                     (V.tail bV) 
--         where
--             curRow = getRow 1 lower 
--             offset = V.length xV
--             lm   =  getRow 1 lower V.! offset
--             curB = V.head bV  
--             negSum = curRow `dotV` xV
--             curX = (curB - negSum) / lm
                    


-- | Solves a linear equality system @A x = b@ given by an upper triangular matrix via
-- backward substitution.
backwardSub :: (Fractional a, Show a) => Matrix a -> V.Vector a -> V.Vector a
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


forwardSub :: (Fractional a, Show a) => Matrix a -> V.Vector a -> V.Vector a
forwardSub lower bV = reverseV $ backwardSub (reverseM lower) (reverseV bV)
    where
        reverseM = fromLists . reverse. (map reverse) . toLists
        reverseV = V.fromList . reverse .V.toList


-- backwardSub :: Fractional a => Matrix a -> V.Vector a -> V.Vector a
-- backwardSub upper bV = forwardSub (reverseM upper) (reverseV bV)
--     where
--         reverseM = fromLists . reverse . toLists
--         reverseV = V.fromList . reverse .V.toList


solveLS :: (Fractional a, Ord a, Show a) => Matrix a -> V.Vector a -> Maybe (V.Vector a)
solveLS matrix vector = Just $ backwardSub u y
    where
        Just (u,l,_,_) = luDecomp matrix
        y = forwardSub l vector



instance Num [Rational]
      
    --(*)  
--    abs = map abs
--    signum = map signum, fromInteger, (negate | (-))

instance Num Bool where
    fromInteger 0 = False
    fromInteger 1 = True
