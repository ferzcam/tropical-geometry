{-# LANGUAGE DataKinds, TypeFamilies, FlexibleContexts, FlexibleInstances, PolyKinds #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}

module Graphics.Drawings where

import Graphics.Gloss

import Polynomial.Monomial
import Polynomial.Prelude
import Polynomial.Hypersurface
import Geometry.ConvexHull2 (Point2D)

fromPoint2D :: Point2D -> Point
fromPoint2D (x,y) = (fromIntegral x, fromIntegral y)

tupleToList :: (Point2D, Point2D) -> [Point]
tupleToList (p1, p2) = [fromPoint2D p1, fromPoint2D p2]

makeFig :: (IsMonomialOrder ord, Ord k, Integral k)  => Polynomial k ord n -> IO ()
makeFig poly = let figure = Pictures $ map (Line . tupleToList) (hypersurface poly) in
    display (InWindow "My Window" (200, 200) (10, 10)) white figure


