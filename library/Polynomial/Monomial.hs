module Polynomial.Monomial 
(
    -- * Types
    Monomial(..),
    Lex,
    Revlex,
    
    -- * Classes
    IsMonomialOrder,
    
)

where

import Data.Function
import Prelude hiding (lex)

-- | Monomial is defined as an array of exponents
data Monomial ord = Monomial {getMonomial :: [Int]} deriving(Eq)
                    
------------------------------------------
showMonomial :: [Int] -> Int -> String
showMonomial [] _ = ""
showMonomial (x:xs) var
    | x == 0 = showMonomial xs (var+1)
    | x == 1 = "X_" ++  (show var) ++ showMonomial xs (var+1)
    | otherwise = "X_" ++  (show var) ++ "^" ++ (show x) ++ showMonomial xs (var+1)


instance Show (Monomial ord) where
    show monomial = showMonomial (getMonomial monomial) 0
------------------------------------------

-- | Definition of what a monomial order must meet
class IsMonomialOrder ord where
    compareMonomial :: Monomial ord -> Monomial ord -> Ordering
-----------------------------

data Lex = Lex -- ^ Just the datatype for Lex ordering
data Revlex = Revlex -- ^ Just the datatype for Revlex ordering

lex :: [Int] -> [Int] -> Ordering
lex [] [] = EQ
lex (x:xs) (y:ys)
    | (x == 0 && y == 0) || x==y = lex xs ys
    | x > y = GT
    | otherwise = LT

revlex :: [Int] -> [Int] -> Ordering
revlex [] [] = EQ
revlex x y
    | (xr == 0 && yr == 0) || xr==yr = revlex (reverse xrs) (reverse yrs)
    | xr > yr = GT 
    | otherwise = LT
    where 
        (xr:xrs) = reverse x
        (yr:yrs) = reverse y

instance IsMonomialOrder Lex where
    compareMonomial m n = (lex `on` getMonomial) m n

instance IsMonomialOrder Revlex where
    compareMonomial m n = (revlex `on` getMonomial) m n

instance (IsMonomialOrder ord) => Ord (Monomial ord) where
    compare = compareMonomial

