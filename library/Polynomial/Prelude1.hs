{-# LANGUAGE DataKinds, TypeFamilies, FlexibleContexts, FlexibleInstances, PolyKinds #-}
{-# LANGUAGE ConstrainedClassMethods, UndecidableInstances, MultiParamTypeClasses #-}


module Polynomial.Prelude1 (
    -- * Types
    Polynomial(..),

    -- * Classes
    IsPolynomial(..)
) where 

import Prelude as P 
import qualified Data.Map.Strict as MS
import Polynomial.Monomial1
import Arithmetic.Numbers
import Numeric.Algebra as NA hiding ((<), (>), (-))
import qualified Numeric.Additive.Class as AD
import Debug.Trace
import qualified Data.Sized as DS
import              GHC.TypeLits
import Data.Type.Ordinal.Builtin


type Index = Int

class    (DecidableZero r, Rig r, Commutative r, Eq r) => CoeffRig r
instance (DecidableZero r, Rig r, Commutative r, Eq r) => CoeffRig r -- ^ Synoym for instances. 


-- | Polynomial requires just the type of the coefficient and the monomial ordering. 
-- | Arity is given when defining variables with 'variable' function
data Polynomial k ord n = Polynomial { terms :: (MS.Map (Monomial ord n) k)} deriving(Eq)

instance (Unital k, Show k, Eq k) => Show (Polynomial k Lex n) where 
    show = dropPlusSign .  showTerms . reverse . MS.toList . terms 

instance (KnownNat n, Unital k, Show k, Eq k) => Show (Polynomial k Revlex n) where 
    show = dropPlusSign .  showTerms . (map reverseMon) . reverse . MS.toList . terms 
    
reverseMon :: (KnownNat n, IsMonomialOrder ord) => (Monomial ord n, a) -> (Monomial ord n, a)
reverseMon (Monomial mon, a) = ((toMonomial . reverse . DS.toList) mon, a) 


dropPlusSign :: String -> String
dropPlusSign [] = error "String too short in dropPlusSign function"
dropPlusSign [_] = error "String too short in dropPlusSign function"
dropPlusSign [_,_] = error "String too short in dropPlusSign function"
dropPlusSign s@(x:y:z:a)
    | (x:y:[z]) == " + " = a
    | otherwise = s

showTerms :: (Unital k, Eq k, Show k) =>  [((Monomial ord n), k)] -> String
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
    type Arity poly :: Nat
    toPolynomial :: (KnownNat n) => (Mon n, Coeff poly) -> poly

    variable :: (Unital (Coeff poly), IsPolynomial poly) => Ordinal n -> SNat n -> poly
    



instance (KnownNat n, IsMonomialOrder ord, CoeffRig k) => IsPolynomial (Polynomial k ord n) where
    type Coeff (Polynomial k ord n) = k
    type Arity (Polynomial k ord n) = n
    toPolynomial (mon, coeff) = Polynomial $ MS.singleton (Monomial mon) coeff

    variable idx arity =
        let (zero1, zero2) = splitAt (idx-1) $ replicate (arity-1) 0
            monomialArray = zero1 ++ [1] ++ zero2
        in toPolynomial (toMonomial monomialArray, one)




instance (IsMonomialOrder ord, KnownNat n) => Num (Polynomial (Tropical Integer) ord n) where 
    (+) (Polynomial terms1) (Polynomial terms2) = Polynomial $ MS.unionWith (P.+) terms1 terms2
    (*) (Polynomial terms1) (Polynomial terms2) = Polynomial $ MS.fromListWith (P.+) [ prodTerm t1 t2 | t1 <- MS.toList terms1, t2 <- MS.toList terms2]
    fromInteger x = Polynomial $ MS.singleton one (Tropical x)
    negate poly =  Polynomial $ MS.map P.negate $ terms poly

instance (Num k, IsMonomialOrder ord) => AD.Additive (Polynomial k ord n) where 
    (+) (Polynomial terms1) (Polynomial terms2) = Polynomial $ MS.unionWith (P.+) terms1 terms2


instance (AD.Additive (Polynomial k ord n), Semiring k, Num k) => LeftModule k (Polynomial k ord n) where
    num .* poly = num !* poly

instance (AD.Additive (Polynomial k ord n), Semiring k, Num k) => RightModule k (Polynomial k ord n) where
    poly *. num = num !* poly



-- | Aux functions

prodTerm :: (KnownNat n, Num k, IsMonomialOrder ord) => (Monomial ord n, k) -> (Monomial ord n, k) -> (Monomial ord n, k)
prodTerm (mon1, coeff1) (mon2, coeff2) = (mon1 NA.* mon2, coeff1 P.* coeff2)

(!*) :: (Num k) => k -> Polynomial k ord n -> Polynomial k ord n
num !* poly = Polynomial $ MS.map (P.* num) (terms poly)
