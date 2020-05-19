{-# LANGUAGE DataKinds, TypeFamilies, FlexibleContexts, FlexibleInstances, PolyKinds #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}

module Polynomial.Hypersurface where

import Geometry.Vertex
import Geometry.Facet (Hyperplane, Facet)
import Polynomial.System
import Polynomial.Prelude
import Polynomial.Monomial
import Geometry.Facet
import Geometry.Polyhedral

import Util
import qualified Data.Map.Strict as MS
import qualified Data.Bimap as BM
import qualified Data.Sized.Builtin as DS
import Data.Matrix hiding (trace)
import Data.Maybe
import Data.List

import Debug.Trace


expVecs :: (IsMonomialOrder ord, Real k, Show k) => Polynomial k ord n -> [Vertex]
expVecs poly = safeZipWith (++) expVec (map return coeffs)
    where
        terms = (MS.toList . getTerms) poly
        expVec = ((map ((map fromIntegral) . DS.toList . getMonomial . fst))) terms
        coeffs = map (toRational.snd) terms


vertices :: (IsMonomialOrder ord, Real k, Show k) => Polynomial k ord n -> [Maybe Vertex]
vertices poly = map solveSystem facetMons
    where
        terms = (MS.toList . getTerms) poly
        points = expVecs poly
        dictPointTerm = MS.fromList $ zip points terms
        extremal = sort $ extremalVertices points
        dictIndexPoint = MS.fromList $ zip [1..] extremal
        facetEnum = facetEnumeration extremal
        subdivision = lowerFacets $ map (\(a,b,_) -> (a,b)) facetEnum
        facetMons = let 
                        lookUp2 = ((MS.!) dictPointTerm) . ((MS.!) dictIndexPoint)
                    in  map ((map lookUp2).fst) subdivision
