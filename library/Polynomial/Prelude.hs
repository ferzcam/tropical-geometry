{-# LANGUAGE DataKinds, TypeFamilies, FlexibleContexts, FlexibleInstances, PolyKinds #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}


--ConstrainedClassMethods, 
module Polynomial.Prelude (
    -- * Types
    Polynomial(..),

    -- * Classes
    IsPolynomial(..),
    IsOrderedPolynomial(..)
) where 

import Control.Lens
import Prelude as P 
import qualified Data.Map.Strict as MS
import Polynomial.Monomial
import Arithmetic.Numbers
import Numeric.Algebra as NA hiding ((<), (>), (-))
import qualified Numeric.Additive.Class as AD
import Debug.Trace
import qualified Data.Sized as DS
import              GHC.TypeLits
import Data.Type.Ordinal.Builtin
import           Data.Singletons.Prelude


type Index = Int

class    (DecidableZero r, Rig r, Commutative r, Eq r) => CoeffRig r

-- | Synonym for instances.
instance (DecidableZero r, Rig r, Commutative r, Eq r) => CoeffRig r  


-- | Polynomial requires just the type of the coefficient and the monomial ordering. 
-- | Arity is given when defining variables with 'variable' function
newtype Polynomial k ord n = Polynomial { getTerms :: MS.Map (Monomial ord n) k} deriving(Eq)

instance (Unital k, Show k, Eq k) => Show (Polynomial k Lex n) where 
    show = dropPlusSign .  showTerms . reverse . MS.toList . getTerms 

instance (KnownNat n, Unital k, Show k, Eq k) => Show (Polynomial k Revlex n) where 
    show = dropPlusSign .  showTerms . map reverseMon . reverse . MS.toList . getTerms 
    
reverseMon :: (KnownNat n, IsMonomialOrder ord) => (Monomial ord n, a) -> (Monomial ord n, a)
reverseMon (Monomial mon, a) = ((toMonomial . reverse . DS.toList) mon, a) 


dropPlusSign :: String -> String
dropPlusSign [] = error "String too short in dropPlusSign function"
dropPlusSign [_] = error "String too short in dropPlusSign function"
dropPlusSign [_,_] = error "String too short in dropPlusSign function"
dropPlusSign s@(x:y:z:a)
    | (x:y:[z]) == " + " = a
    | otherwise = s

showTerms :: (Unital k, Eq k, Show k) =>  [(Monomial ord n, k)] -> String
showTerms [] = ""
showTerms (t:ts)
    | coeff == one = " + " ++ show mon ++ showTerms ts
    | otherwise = " + " ++ show coeff ++ show mon ++ showTerms ts
    where 
        coeff = snd t
        mon = fst t


-- | Every polynomial must implement this class
class (CoeffRig (Coeff poly), KnownNat (Arity poly)) => IsPolynomial poly where
    type Coeff poly :: *
    type Arity poly :: Nat
    
    arity :: poly -> SNat (Arity poly)

    toPolynomial :: (Mon (Arity poly), Coeff poly) -> poly
    
    fromMonomial :: Mon (Arity poly) -> poly
    fromMonomial mon = toPolynomial (mon, one)

    variable :: Ordinal (Arity poly) -> poly
    variable idx = fromMonomial $ DS.replicate sing 0 & ix idx .~ 1  
    

class (IsMonomialOrder (MonOrder poly), IsPolynomial poly) => IsOrderedPolynomial poly where
    type MonOrder poly :: *
    
    terms :: poly -> MS.Map (Monomial (MonOrder poly) (Arity poly)) (Coeff poly)

    leadingTerm :: poly -> (Coeff poly, Monomial (MonOrder poly) (Arity poly))
    leadingTerm = (,) <$> leadingCoeff <*> leadingMonomial
    leadingMonomial :: poly -> Monomial (MonOrder poly) (Arity poly)
    leadingMonomial = snd . leadingTerm

    leadingCoeff :: poly -> Coeff poly
    leadingCoeff = fst . leadingTerm

instance (KnownNat n, IsMonomialOrder ord, CoeffRig k) => IsPolynomial (Polynomial k ord n) where
    type Coeff (Polynomial k ord n) = k
    type Arity (Polynomial k ord n) = n
    
    arity = DS.sLength . getMonomial . leadingMonomial
    toPolynomial (mon, coeff) = Polynomial $ MS.singleton (Monomial mon) coeff
    

instance (KnownNat n, CoeffRig k, IsMonomialOrder ord) => IsOrderedPolynomial (Polynomial k ord n) where
   type MonOrder (Polynomial k ord n) = ord
   terms = getTerms
   leadingTerm (Polynomial d) = case MS.lookupMax d of
                                   Just (mon, coeff) -> (coeff, mon)
                                   Nothing -> (one, one)



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
num !* poly = Polynomial $ MS.map (P.* num) (getTerms poly)
