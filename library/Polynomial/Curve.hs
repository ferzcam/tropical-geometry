{-# LANGUAGE DataKinds #-}
-- | Connect the package's polynomial representation to exact tropical curves.
module Polynomial.Curve (tropicalCurveOf) where

import Arithmetic.Numbers (Tropical(..))
import qualified Data.Map.Strict as Map
import qualified Data.Sized as Sized
import Geometry.TropicalCurve (Curve, Term(..), tropicalCurve)
import Polynomial.Monomial (getMonomial)
import Polynomial.Prelude (Polynomial(..))

-- | Compute the min-plus root locus of a bivariate polynomial. Infinite
-- coefficients are absent terms, not coefficients equal to zero. An all-Inf
-- or empty polynomial is rejected because its everywhere-infinite value has
-- no finite curve representation. Finite coefficients are converted directly
-- using their underlying 'Real' instance, preserving exact integer and
-- rational values. Exponents retain the polynomial type's existing Int range.
tropicalCurveOf :: Real a => Polynomial (Tropical a) ord 2 -> Either String Curve
tropicalCurveOf polynomial = do
    finite <- traverse convert [ (mon,c) | (mon,Tropical c) <- Map.toList (getTerms polynomial) ]
    tropicalCurve finite
  where
    convert (mon,c) = case Sized.toList (getMonomial mon) of
        [x,y] -> Right (Term (toInteger x) (toInteger y) (toRational c))
        _ -> Left "A tropical curve requires exactly two polynomial variables."
