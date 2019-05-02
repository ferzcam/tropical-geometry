module Arithmetic.Matrix where

import Numeric.Algebra hiding ((+), (*))
import Data.List
import Data.Function (on)
-- import Data.Matrix hiding (transpose, zero)

data TMatrix a = TMatrix {toList :: [[a]]} -- ^ "a" should be Tropical
    deriving (Eq, Show)

-- | This should be fixed with Functor instance
fmap' :: ([[a]] -> [[b]]) -> TMatrix a -> TMatrix b
fmap' f (TMatrix a) = TMatrix (f a)

transp :: TMatrix a -> TMatrix a
transp tm = fmap' transpose tm

msum :: (Num a) => TMatrix a-> TMatrix a -> TMatrix a
msum a b = TMatrix $ (zipWith (zipWith (+)) `on` toList) a b

mmult :: (Rig a, Num a) => TMatrix a -> TMatrix a -> TMatrix a 
mmult a b = TMatrix $ [[ foldr (+) zero $ zipWith (*) ar bc | bc <- (transpose (toList b))] | ar <- (toList a) ]


instance (Num a, Rig a) => Num (TMatrix a) where
    (+) = msum
    (*) = mmult