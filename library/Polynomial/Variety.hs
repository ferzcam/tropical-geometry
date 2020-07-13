{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE TypeFamilies #-}


-- {-# LANGUAGE DataKinds, TypeFamilies, FlexibleContexts, FlexibleInstances, PolyKinds #-}
-- {-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}

module Polynomial.Variety where

import Polynomial.Prelude
import Polynomial.Monomial
import Geometry.Vertex
import Util


import Prelude hiding ((+), (*), (^))
import Numeric.Algebra hiding (fromInteger)
import qualified Data.Sized.Builtin as DS
import qualified Data.Map.Strict as MS
import Control.Arrow
import Data.Singletons
import GHC.Natural
import GHC.TypeLits
import Data.List

-- import Debug.Trace

eval :: (KnownNat n, IsMonomialOrder ord, CoeffRig k, Division k, Num k) => [k] -> Polynomial k ord n -> k
eval locus poly
    | length locus /= (naturalToInt . fromSing) (arity poly) = error "eval: point must have the same dimension as the arity of the polynomial."
    | otherwise = foldr1 (+) $ map (uncurry (*)) monsEvald
        where
            listTerms = (((map.first) (DS.toList . getMonomial)) . MS.toList . getTerms) poly
            monsEvald = (map.first) ((foldr1 (*)) . (safeZipWith (^) locus)) listTerms


isSolution :: (KnownNat n, IsMonomialOrder ord, Show k, CoeffRig k, Division k, Num k) 
    => [k] -> Polynomial k ord n -> Bool
isSolution locus poly = timesAchieved >= 2
    where
        listTerms = (((map.first) (DS.toList . getMonomial)) . MS.toList . getTerms) poly
        monsEvald = (map.first) ((foldr1 (*)) . (safeZipWith (^) locus)) listTerms
        termsEvald = map (uncurry (*)) monsEvald
        minimum = foldr1 (+) termsEvald
        timesAchieved = foldr (\m acc -> if m == minimum then acc+1 else acc) (0 :: Int) termsEvald


isRay :: 
            [Rational] -- | Vertex
        ->  [Integer] -- | Primitive vector emanating from thr vertex
        ->  [Vertex]
        ->  Bool
isRay vertex prim vertices = not $ or $ safeZipWith (==) vertices' verticesFromParam
    where
        vertices' = delete vertex vertices
        parameters_t = map (getParameter vertex (prim)) vertices'
        paramLine = line vertex prim
        verticesFromParam = map (\t -> if t Prelude.< 0 then vertex else paramLine <*> (pure t)) parameters_t