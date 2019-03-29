{-# LANGUAGE FlexibleInstances #-}

module Arithmetic.Numbers (Tropical(..)) where

import Data.Function
import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Additive.Class


--The set includes all the real numbers and the inifinity
data Tropical a = Tropical {value :: a} | Inf 
  deriving (Eq)

instance (Show a) => Show (Tropical a) where
  show Inf = "Inf"
  show (Tropical a) = show a



(.+.) :: (Ord a) => Tropical a -> Tropical a -> Tropical a
Inf .+. t = t
t .+. Inf = t
t1 .+. t2 = Tropical $ (min `on` value) t1 t2 

(.*.) :: (Num a) => Tropical a -> Tropical a -> Tropical a
t .*. Inf = Inf
Inf .*. t = Inf
t1 .*. t2 = Tropical $ ((Prelude.+) `on` value) t1 t2 

instance (Real a) => Monoid (Tropical a) where
  mempty = Inf

instance (Num a) => Unital (Tropical a) where
  one = Tropical 0

instance (Num a) => Multiplicative (Tropical a) where 
  (*) = (.*.)

instance Additive (Tropical a)
instance Abelian (Tropical a)
instance (Num a) => Semiring (Tropical a) 

instance Num (Tropical Integer) where
  (+) = (.+.)
  (*) = (.*.)
  fromInteger a = Tropical a
  negate = fmap negate

instance Functor Tropical where
  fmap f (Tropical a) = Tropical (f a)