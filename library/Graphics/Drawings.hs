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

makeFig :: (IsMonomialOrder ord, Ord k, Integral k)  => Polynomial k ord n  -> IO ()
makeFig poly = let 
                    figure = color blue $ Pictures $ map (Line . tupleToList) (hypersurface poly)
                    scaled = scale 10.0 10.0 figure 
                in
    display (InWindow "My Window" (400, 400) (10, 10)) white scaled


makeFigs :: (IsMonomialOrder ord, Ord k, Integral k) => [Polynomial k ord n] -> IO()
makeFigs polys = let
                    colors = [red, green, blue, yellow, cyan, magenta, rose, violet, azure, aquamarine, chartreuse, orange]
                    figures = map (Pictures . map (Line . tupleToList) . hypersurface) polys
                    putColors = zipWith color colors figures
                    scaled = scale 10.0 10.0 (Pictures putColors)
                in
    display (InWindow "Tropical curves" (400, 400) (10,10)) white scaled
