{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}

module Polynomial.Prelude (
    -- * Types
    Polynomial(..),
    variable 
) where 

import Prelude hiding (negate)
import qualified Data.Map.Strict as MS
import Polynomial.Monomial
import Data.List
import GHC.Natural
import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Additive.Class


type Index = Int
type Arity = Int

-- | Polynomial requires just the type of the coefficient and the monomial ordering. 
-- Arity is given when defining variables with 'variable' function
data Polynomial k ord = Polynomial { terms :: (MS.Map (Monomial ord) k)} deriving(Eq)

instance (Unital k, Show k, Eq k) => Show (Polynomial k Lex) where 
    show = dropPlusSign .  showTerms . reverse . MS.toList . terms 

instance (Unital k, Show k, Eq k) => Show (Polynomial k Revlex) where 
    show = dropPlusSign .  showTerms . (map reverseMon) . reverse . MS.toList . terms 
    
reverseMon :: (Monomial ord, a) -> (Monomial ord, a)
reverseMon (Monomial mon, a) = (Monomial $ reverse mon, a) 


dropPlusSign :: String -> String
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
class IsPolynomial poly where
    type Coeff poly :: *
    toPolynomial :: ([Int], Coeff poly) -> poly

instance (IsMonomialOrder ord) => IsPolynomial (Polynomial k ord) where
    type Coeff (Polynomial k ord) = k
    toPolynomial (mon, coeff) = Polynomial $ MS.singleton (Monomial mon) coeff


variable :: (Unital (Coeff poly), IsPolynomial poly) => Index -> Arity -> poly
variable idx arity
    | idx < 1 || idx > arity = error "Index of variable not allowed. Check variables definitions"
    | otherwise = 
        let (zero1, zero2) = splitAt (idx-1) $ replicate (arity-1) 0
            monomialArray = zero1 ++ [1] ++ zero2
        in toPolynomial (monomialArray, one)


