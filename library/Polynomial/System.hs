{-# LANGUAGE DataKinds, TypeFamilies, FlexibleContexts, FlexibleInstances, PolyKinds #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}

module Polynomial.System where

import Polynomial.Monomial
import qualified Data.Vector as V
import qualified Data.Sized.Builtin as DS
import Util
import Data.Matrix hiding (trace)
import Debug.Trace
import Data.List
import Data.Function
import Control.Arrow

solveSystem :: (IsMonomialOrder ord, Real k, Show k) => [(Monomial ord n, k)] -> Maybe [Rational]
solveSystem terms =  fmap V.toList $ solveLS (fromLists leftSide) (V.fromList rightSide)
    where
        arity' = (length . DS.toList . getMonomial . fst . head) terms 
        termsInNumbers = take (arity'+1) $ map (((map fromIntegral).DS.toList.getMonomial.fst) &&& (toRational.negate.snd)) terms
        mons = map fst termsInNumbers
        coeffs = map snd termsInNumbers
        leftSide = map ($-$ (head mons)) (tail mons)
        rightSide =  map ((flip (-) (head coeffs))) (tail coeffs)
    
        