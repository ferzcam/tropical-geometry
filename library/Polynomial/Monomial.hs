{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, PolyKinds, TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Polynomial.Monomial
(
    -- * Types
    Monomial(..),
    Mon,
    SNat,
    Lex,
    Revlex,
    
    -- * Classes
    IsMonomialOrder,
    
    -- * Functions
    toMonomial

)

where

import Data.Function
import Numeric.Algebra hiding ((+),(>))
import Prelude hiding (lex)
import qualified Data.Sized as DS
import qualified Data.Sequence as Seq
import           Data.Singletons.Prelude
import              GHC.TypeLits
import Control.Lens (makeLenses, makeWrapped)



type SNat (n :: Nat) = Sing n

type Sized' n a = DS.Sized Seq.Seq n a
type Mon n = Sized' n Int

-- | Monomial is defined as an array of exponents
newtype Monomial ord n = Monomial {getMonomial :: Mon n} deriving(Eq)
 

------------------------------------------
makeLenses ''Monomial
makeWrapped ''Monomial

showMonomial :: [Int] -> Int -> String
showMonomial [] _ = ""
showMonomial (x:xs) var
    | x == 0 = showMonomial xs (var+1)
    | x == 1 = "X_" ++  show var ++ showMonomial xs (var+1)
    | otherwise = "X_" ++  show var ++ "^" ++ show x ++ showMonomial xs (var+1)


instance Show (Monomial ord n) where
    show monomial = showMonomial (DS.toList $ getMonomial monomial) 0
------------------------------------------

-- | Definition of what a monomial order must meet
class IsMonomialOrder (ord :: *) where
    compareMonomial :: Monomial ord n -> Monomial ord n -> Ordering
-----------------------------

data Lex = Lex -- ^ Just the datatype for Lex ordering
data Revlex = Revlex -- ^ Just the datatype for Revlex ordering

lex :: Monomial ord n -> Monomial ord n -> Ordering
lex = lex' `on` (DS.toList . getMonomial)

lex' :: [Int] -> [Int] -> Ordering
lex' [] [] = EQ
lex' [] _ = LT
lex' _ [] = GT
lex' (x:xs) (y:ys)
    | (x == 0 && y == 0) || x==y = lex' xs ys
    | x > y = GT
    | otherwise = LT


revlex :: Monomial ord n -> Monomial ord n -> Ordering
revlex= revlex' `on` (DS.toList . getMonomial)
 
revlex' :: [Int] -> [Int] -> Ordering
revlex' [] [] = EQ
revlex' [] _ = LT
revlex' _ [] = GT
revlex' x y
    | (xr == 0 && yr == 0) || xr==yr = revlex' (reverse xrs) (reverse yrs)
    | xr > yr = GT 
    | otherwise = LT
    where 
        (xr:xrs) = reverse x
        (yr:yrs) = reverse y


-- | convert NAry list into Monomial.
fromList :: SNat n -> [Int] -> Mon n
fromList len = DS.fromListWithDefault len 0

toMonomial :: (IsMonomialOrder ord, KnownNat n) => [Int] -> Monomial ord n
toMonomial a = Monomial $ fromList sing a

instance IsMonomialOrder Lex where
    compareMonomial = lex

instance IsMonomialOrder Revlex where
    compareMonomial = revlex

instance (IsMonomialOrder ord) => Ord (Monomial ord n) where
    compare = compareMonomial

instance (IsMonomialOrder ord, KnownNat n) => Unital (Monomial ord n) where
  one = toMonomial []

instance (IsMonomialOrder ord, KnownNat n) => Multiplicative (Monomial ord n) where 
    (*) = prodMon
  
prodMon :: (IsMonomialOrder ord, KnownNat n) => Monomial ord n -> Monomial ord n -> Monomial ord n
prodMon mon1 mon2
    | mon1 == one = mon2
    | mon2 == one = mon1
    | otherwise = toMonomial $ (zipWith (+) `on` (DS.toList . getMonomial)) mon1 mon2

