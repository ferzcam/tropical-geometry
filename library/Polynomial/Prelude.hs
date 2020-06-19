{-# LANGUAGE DataKinds, TypeFamilies, FlexibleContexts, FlexibleInstances, PolyKinds #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, NoImplicitPrelude #-}



module Polynomial.Prelude (
    -- * Types
    Polynomial(..),

    -- * Classes
    IsPolynomial(..),
    IsOrderedPolynomial(..),
    CoeffRig(..),

    -- * Functions
    (!*)

) where 

import Prelude 
    (Eq, 
    Num, 
    Show, 
    String, 
    Maybe(..), 
    Integer, 
    show, 
    fromInteger,
    (.), error, (==), (&&), otherwise, fst, snd, ($), (<$>), (<*>), (+), (*), negate)
import Control.Lens
import qualified Data.Map.Strict as MS
import Numeric.Algebra as NA
import qualified Numeric.Additive.Class as Ad
import Debug.Trace
import Data.List
import qualified Data.Sized as DS
import GHC.TypeLits
import Data.Type.Ordinal.Builtin
import Data.Singletons.Prelude


import Polynomial.Monomial
import Arithmetic.Numbers



-- infix 7 !*!


class (DecidableZero r, Rig r, Commutative r, Eq r) => CoeffRig r

-- | Synonym for instances.
instance (DecidableZero r, Rig r, Commutative r, Eq r) => CoeffRig r  


-- | Polynomial requires just the type of the coefficient and the monomial ordering. 
-- | Arity is given when defining variables with 'variable' function
newtype Polynomial k ord n = Polynomial { getTerms :: MS.Map (Monomial ord n) k} deriving (Eq)

instance (Unital k, Show k, Eq k, Unital (Monomial Lex n)) => Show (Polynomial k Lex n) where 
    show = dropPlusSign .  showTerms . reverse . MS.toList . getTerms 

instance (KnownNat n, Unital k, Show k, Eq k, Unital (Monomial Revlex n)) => Show (Polynomial k Revlex n) where 
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

showTerms :: (Unital k, Eq k, Show k, Unital (Monomial ord n)) =>  [(Monomial ord n, k)] -> String
showTerms [] = ""
showTerms (t:ts)
    | coeff == one && mon == one = " + " ++ show coeff
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

    -- (!*!) :: Coeff poly -> poly -> poly
    -- (!*!) = (!*)
    

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
    

-- instance (KnownNat n, IsMonomialOrder ord) => IsPolynomial (Polynomial (Tropical Integer) ord n) where
--     type Coeff (Polynomial (Tropical Integer) ord n) = Tropical Integer
--     type Arity (Polynomial (Tropical Integer) ord n) = n
    
--     arity = DS.sLength . getMonomial . leadingMonomial
--     toPolynomial (mon, coeff) = Polynomial $ MS.singleton (Monomial mon) coeff

-- instance (KnownNat n, IsMonomialOrder ord) => IsOrderedPolynomial (Polynomial (Tropical Integer) ord n) where
--    type MonOrder (Polynomial (Tropical Integer) ord n) = ord
--    terms = getTerms
--    leadingTerm (Polynomial d) = case MS.lookupMax d of
--                                    Just (mon, coeff) -> (coeff, mon)
--                                    Nothing -> (one, one)

instance (KnownNat n, CoeffRig k, IsMonomialOrder ord) => IsOrderedPolynomial (Polynomial k ord n) where
   type MonOrder (Polynomial k ord n) = ord
   terms = getTerms
   leadingTerm (Polynomial d) = case MS.lookupMax d of
                                   Just (mon, coeff) -> (coeff, mon)
                                   Nothing -> (one, one)

instance (KnownNat n, IsMonomialOrder ord) => Num (Polynomial (Tropical Integer) ord n) where 
    -- (+) = ($+$)
    -- (*) = ($*$)
    fromInteger x = Polynomial $ MS.singleton one (NA.fromInteger x)
    negate poly =  Polynomial $ MS.map Prelude.negate $ terms poly
    
-- instance (IsMonomialOrder ord, KnownNat n) => Additive (Polynomial (Tropical Integer) ord n) where 
--     (+) (Polynomial terms1) (Polynomial terms2) = Polynomial $ MS.unionWith     (+) terms1 terms2
--     (*) (Polynomial terms1) (Polynomial terms2) = Polynomial $ MS.fromListWith (+) [ prodTerm t1 t2 | t1 <- MS.toList terms1, t2 <- MS.toList terms2]
--     fromInteger x = Polynomial $ MS.singleton one (fromInteger x)
--     negate poly =  Polynomial $ MS.map negate $ terms poly
    
-- instance (IsMonomialOrder ord, KnownNat n) => P.Fractional (Polynomial (Tropical Integer) ord n) where
--     recip (Polynomial terms1) = Polynomial $ (MS.fromList . map negateTerm . MS.toList) terms1  -- Pseudo recip, only work with monic polynomials
--         where
--             negateTerm (mon, coef) = let 
--                                         listExps = DS.toList $ getMonomial mon
--                                         negated = map negate listExps
--                                     in (toMonomial negated, coef) 

instance (IsMonomialOrder ord, KnownNat n) => Division (Polynomial (Tropical Integer) ord n) where
    recip = recip'

instance (Additive k, IsMonomialOrder ord) => Additive (Polynomial k ord n) where 
    (+) = ($+$)

instance (Additive k, Multiplicative k, IsMonomialOrder ord, KnownNat n) => Multiplicative (Polynomial k ord n) where 
    (*) = ($*$)

instance (Additive (Polynomial k ord n), Semiring k, Additive k) => LeftModule k (Polynomial k ord n) where
    num .* poly = num !* poly

instance (Additive (Polynomial k ord n), Semiring k, Additive k) => RightModule k (Polynomial k ord n) where
    poly *. num = num !* poly

instance (Multiplicative (Polynomial k ord n), IsMonomialOrder ord, KnownNat n, Unital k) 
    => Unital (Polynomial k ord n) where
  one = Polynomial $ MS.fromList [(toMonomial [],one)]
-- | Aux functions

prodTerm :: (KnownNat n, Additive k, Multiplicative k, IsMonomialOrder ord) => (Monomial ord n, k) -> (Monomial ord n, k) -> (Monomial ord n, k)
prodTerm (mon1, coeff1) (mon2, coeff2) = (mon1 NA.* mon2, coeff1 NA.* coeff2)

(!*) :: (Additive k, Multiplicative k) => k -> Polynomial k ord n -> Polynomial k ord n
num !* poly = Polynomial $ MS.map (NA.* num) (getTerms poly)


($+$) :: (Additive k, IsMonomialOrder ord) => Polynomial k ord n -> Polynomial k ord n -> Polynomial k ord n
($+$) (Polynomial terms1) (Polynomial terms2) = Polynomial $ MS.unionWith (Ad.+) terms1 terms2

($*$) :: (KnownNat n, Additive k, Multiplicative k, IsMonomialOrder ord) 
    => Polynomial k ord n -> Polynomial k ord n -> Polynomial k ord n
($*$) (Polynomial terms1) (Polynomial terms2) = Polynomial $ MS.fromListWith (Ad.+) [ prodTerm t1 t2 | t1 <- MS.toList terms1, t2 <- MS.toList terms2]

recip' :: (IsMonomialOrder ord, KnownNat n) => Polynomial k ord n -> Polynomial k ord n
recip' (Polynomial terms1) = Polynomial $ (MS.fromList . map negateTerm . MS.toList) terms1  -- Pseudo recip, only work with monic polynomials
    where
        negateTerm (mon, coef) = let 
                                    listExps = DS.toList $ getMonomial mon
                                    negated = map Prelude.negate listExps
                                in (toMonomial negated, coef) 