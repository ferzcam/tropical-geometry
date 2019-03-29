{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Polynomial.Numeric (
    
)where

import Polynomial.Prelude
import Polynomial.Monomial
import qualified Numeric.Algebra as NA
import Numeric.Algebra.Class hiding((*))
import qualified Numeric.Additive.Class as AD
import Arithmetic.Numbers
import Data.Map.Strict as MS

prodTerm :: (Num k) => (Monomial ord, k) -> (Monomial ord, k) -> (Monomial ord, k)
prodTerm (Monomial mon1, coef1) (Monomial mon2, coef2) = (Monomial $ zipWith (+) mon1 mon2, coef1*coef2)  

instance (IsMonomialOrder ord) => Num (Polynomial (Tropical Integer) ord) where 
    (+) (Polynomial terms1) (Polynomial terms2) = Polynomial $ MS.unionWith (+) terms1 terms2
    (*) (Polynomial terms1) (Polynomial terms2) = Polynomial $ MS.fromListWith (*) [ prodTerm t1 t2 | t1 <- MS.toList terms1, t2 <- MS.toList terms2]
    fromInteger x = Polynomial $ MS.singleton (Monomial $ repeat 0) (Tropical x)
    negate poly =  Polynomial $ MS.map negate $ terms poly

instance AD.Additive (Polynomial k ord)



instance (AD.Additive (Polynomial k ord), Semiring k, Num k) => LeftModule k (Polynomial k ord) where
    num .* poly = num !* poly

instance (AD.Additive (Polynomial k ord), Semiring k, Num k) => RightModule k (Polynomial k ord) where
    poly *. num = num !* poly


(!*) :: (Num k) => k -> Polynomial k ord -> Polynomial k ord 
num !* poly = Polynomial $ MS.map (Prelude.* num) (terms poly)