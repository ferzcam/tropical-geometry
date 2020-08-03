{-# LANGUAGE DataKinds, TypeFamilies, FlexibleContexts, FlexibleInstances, PolyKinds #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}

module Graphics.Drawings where

import Graphics.Gloss

import Polynomial.Monomial
import Polynomial.Prelude
import Polynomial.Hypersurface
import Geometry.Vertex
import Data.Map (toList)

-- fromPoint2D :: Point2D -> Point
-- fromPoint2D (x,y) = (fromIntegral x, fromIntegral y)


-- tupleToList :: (Point2D, Point2D) -> [Point]
-- tupleToList (p1, p2) = [fromPoint2D p1, fromPoint2D p2]

fromVertex :: Vertex -> Point
fromVertex vert = (fromRational (vert!!0), fromRational (vert!!1))

rayToList :: (Vertex, [IVertex]) -> [[Point]]
rayToList (vert,rays) = points
    where 
        endPoint lambda ray = zipWith (\v r -> v + lambda*r) vert ray
        points = map (\ray -> [fromVertex vert, fromVertex (endPoint 90 (map toRational ray))] ) rays


fromHyperToList :: Hypersurface -> [[Point]]
fromHyperToList hyp@(HyperS{vertHyp= _, edHyp = edges, rays= rays }) = edgToPoints ++ raysToPoints
    where
        edgToPoints = map (\ed@(Internal (ini,end)) -> [fromVertex ini, fromVertex end] ) edges
        raysToPoints = foldl1 (++) $ (map (rayToList). toList) rays 



makeFig :: (IsMonomialOrder ord, Ord k, Integral k, Show k)  => Polynomial k ord n  -> IO ()
makeFig poly = let 
                    figure = color blue $ Pictures $ map (Line ) (fromHyperToList $ snd $ hypersurface poly)
                    scaled = scale 10.0 10.0 figure 
                in
    display (InWindow "My Window" (400, 400) (10, 10)) white scaled


-- makeFigs :: (IsMonomialOrder ord, Ord k, Integral k) => [Polynomial k ord n] -> IO()
-- makeFigs polys = let
--                     colors = [red, green, blue, yellow, cyan, magenta, rose, violet, azure, aquamarine, chartreuse, orange]
--                     figures = map (Pictures . map (Line . tupleToList) . hypersurface) polys
--                     putColors = zipWith color colors figures
--                     scaled = scale 10.0 10.0 (Pictures putColors)
--                 in
--     display (InWindow "Tropical curves" (400, 400) (10,10)) white scaled
