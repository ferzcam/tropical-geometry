{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods, UndecidableInstances, MultiParamTypeClasses #-}


module Polynomial.Prelude (
    -- * Types
    Polynomial(..),

    -- * Classes
    IsPolynomial(..)
) where 

import Prelude as P 
import qualified Data.Map.Strict as MS
import Polynomial.Monomial
import Arithmetic.Numbers
import Numeric.Algebra as NA hiding ((<), (>), (-))
import qualified Numeric.Additive.Class as AD
import Debug.Trace

type Index = Int
type Arity = Int

class    (DecidableZero r, Rig r, Commutative r, Eq r) => CoeffRig r
instance (DecidableZero r, Rig r, Commutative r, Eq r) => CoeffRig r -- ^ Synoym for instances. 


-- | Polynomial requires just the type of the coefficient and the monomial ordering. 
-- | Arity is given when defining variables with 'variable' function
data Polynomial k ord = Polynomial { terms :: (MS.Map (Monomial ord) k)} deriving(Eq)

instance (Unital k, Show k, Eq k) => Show (Polynomial k Lex) where 
    show = dropPlusSign .  showTerms . reverse . MS.toList . terms 

instance (Unital k, Show k, Eq k) => Show (Polynomial k Revlex) where 
    show = dropPlusSign .  showTerms . (map reverseMon) . reverse . MS.toList . terms 
    
reverseMon :: (Monomial ord, a) -> (Monomial ord, a)
reverseMon (Monomial mon, a) = (Monomial $ reverse mon, a) 


dropPlusSign :: String -> String
dropPlusSign [] = error "String too short in dropPlusSign function"
dropPlusSign [_] = error "String too short in dropPlusSign function"
dropPlusSign [_,_] = error "String too short in dropPlusSign function"
dropPlusSign s@(x:y:z:a)
    | (x:y:[z]) == " + " = a
    | otherwise = s

showTerms :: (Unital k, Eq k, Show k) =>  [((Monomial ord), k)] -> String
showTerms [] = ""
showTerms (t:ts)
    | coeff == one = " + " ++ show mon ++ showTerms ts
    | otherwise = " + " ++ show coeff ++ show mon ++ showTerms ts
    where 
        coeff = snd t
        mon = fst t


-- | Every polynomial must implement this class
class (CoeffRig (Coeff poly)) => IsPolynomial poly where
    type Coeff poly :: *
    toPolynomial :: ([Int], Coeff poly) -> poly

    variable :: (Unital (Coeff poly), IsPolynomial poly) => Index -> Arity -> poly
    variable idx arity
        | idx < 1 || idx > arity = error "Index of variable not allowed. Check variables definitions"
        | otherwise = 
            let (zero1, zero2) = splitAt (idx-1) $ replicate (arity-1) 0
                monomialArray = zero1 ++ [1] ++ zero2
            in toPolynomial (monomialArray, one)



instance (IsMonomialOrder ord, CoeffRig k) => IsPolynomial (Polynomial k ord) where
    type Coeff (Polynomial k ord) = k
    toPolynomial (mon, coeff) = Polynomial $ MS.singleton (Monomial mon) coeff



instance (IsMonomialOrder ord) => Num (Polynomial (Tropical Integer) ord) where 
    (+) (Polynomial terms1) (Polynomial terms2) = Polynomial $ MS.unionWith (P.+) terms1 terms2
    (*) (Polynomial terms1) (Polynomial terms2) = Polynomial $ MS.fromListWith (P.+) [ prodTerm t1 t2 | t1 <- MS.toList terms1, t2 <- MS.toList terms2]
    fromInteger x = Polynomial $ MS.singleton one (Tropical x)
    negate poly =  Polynomial $ MS.map P.negate $ terms poly

instance (Num k, IsMonomialOrder ord) => AD.Additive (Polynomial k ord) where 
    (+) (Polynomial terms1) (Polynomial terms2) = Polynomial $ MS.unionWith (P.+) terms1 terms2


instance (AD.Additive (Polynomial k ord), Semiring k, Num k) => LeftModule k (Polynomial k ord) where
    num .* poly = num !* poly

instance (AD.Additive (Polynomial k ord), Semiring k, Num k) => RightModule k (Polynomial k ord) where
    poly *. num = num !* poly



-- | Aux functions

prodTerm :: (Num k, IsMonomialOrder ord) => (Monomial ord, k) -> (Monomial ord, k) -> (Monomial ord, k)
prodTerm (mon1, coeff1) (mon2, coeff2) = (mon1 NA.* mon2, coeff1 P.* coeff2)

(!*) :: (Num k) => k -> Polynomial k ord -> Polynomial k ord 
num !* poly = Polynomial $ MS.map (P.* num) (terms poly)
