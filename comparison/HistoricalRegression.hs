-- | Regression evidence separating historical failures from corrected adapters.
--
-- Private graphHypersurface/getHyper/raysToMap/isInternal/fromEdgeHyper bodies
-- below are copied (trailing whitespace removed) from the historical file
-- library/Polynomial/Hypersurface.hs at 25892444db7813bb1819df344d36e1fe93400198 (origin/generalTropHyp).
-- Their bugs are INTENTIONAL here: the baseline checks demonstrate the old
-- failure, rather than enshrining that output as correct tropical geometry.
--
-- The corrected reconstructed Yang/GLPK and independent LRS adapters are
-- checked separately against hand-derived geometry and the exact reference.
-- This does not claim the unchanged historical wrapper returns correct output.
module HistoricalRegression (historicalChecks) where

import qualified Data.Map.Strict as MS
import Data.List
import Geometry.Vertex (Vertex, IVertex, standard)
import Geometry.Facet (Hyperplane,facetEnumeration')
import Geometry.TropicalSlice (Term3(..))
import HullSkeleton3 (originalSkeleton3,lrsSkeleton3)
import Skeleton3 (Edge3(..),Skeleton3,canonicalSkeleton3,exactSkeleton3)

-- Minimal representations needed to execute the unchanged historical bodies.
data EdgeHypersurface = Internal (Vertex,Vertex) | External (Vertex,IVertex) deriving Show
instance Eq EdgeHypersurface where
    (==) (Internal (a,b)) (Internal (c,d)) = (a==c && b==d) || (a==d && b==c)
    (==) (External x) (External y) = x==y
    (==) _ _ = False
data Hypersurface = HyperS {vertHyp::[Vertex],edHyp::[EdgeHypersurface],rays::MS.Map Vertex [IVertex]} deriving Show

graphHypersurface :: MS.Map Vertex [([IVertex], Hyperplane)] -> [EdgeHypersurface]
graphHypersurface dictionary = MS.foldrWithKey analyzeCell [] dictionary
    where
        analyzeCell vertex cell edges = (findEdges (MS.toList (MS.delete vertex dictionary)) cell) ++ edges
            where
                findEdges [] [] = []
                findEdges [] c = map (\(_, hyper) -> External (vertex, ((map negate) . standard) hyper)) c
                --    | length cell + 1 == length c = error "graphHypersurface.analyzeCell.findEdges: cell must produce at least one internal edge"
                    -- | otherwise =
                findEdges ((v2,c2):xs) c
                    | length adjacent == 1 = [Internal (vertex,v2)] ++ findEdges xs (delete (fst $ head adjacent) c)
                    | length adjacent == 0 = findEdges xs c
                    | otherwise = error "graphHypersurface.analyzeCell.findEdges: adjacent cells have only ONE hyperplane in common."
                        where
                            adjacent = [(h1, h2) | h1 <- c, h2 <- c2, (sort.fst) h1 == (sort.fst) h2, snd h1 == ((map negate).snd) h2]


getHyper :: [EdgeHypersurface] -> Hypersurface
getHyper edgeHyp = HyperS{vertHyp=vert, edHyp= edges, rays = rays}
    where
        vert = (nub . foldl1 (++) . map (fromEdgeHyper) ) edgeHyp
        edges =  (nub . filter (isInternal)) edgeHyp
        preRays = filter (\ed -> not $ isInternal ed) edgeHyp
        rays = raysToMap preRays MS.empty


raysToMap :: [EdgeHypersurface] -> MS.Map Vertex [IVertex] -> MS.Map Vertex [IVertex]
raysToMap [r@( External (pos, ray)) ] prevMap = MS.insertWith (++) pos [ray] prevMap
raysToMap (r@( External (pos, ray)):rs) prevMap = raysToMap rs newMap
    where
        newMap = MS.insertWith (++) pos [ray] prevMap


isInternal :: EdgeHypersurface -> Bool
isInternal (Internal _ ) = True
isInternal (External _ ) = False


fromEdgeHyper :: EdgeHypersurface -> [Vertex]
fromEdgeHyper (Internal (ini, out)) = [ini]++[out]
fromEdgeHyper (External (vert, ray)) = [vert] ++ [map toRational ray]

-- | Known-bad historical baselines plus corrected hand-geometry regressions.
historicalChecks :: [(String,Bool)]
historicalChecks =
    [ ("Historical failure baseline: ray directions contaminate vertex list",
         sort (vertHyp historicalSimplex) == sort (simplexVertex : map (map fromInteger) simplexDirections)
         && length (vertHyp historicalSimplex) == 5)
    , ("Historical failure baseline: unequal normal scales omit bounded edge",
         sharedNormals lowerFacets == [[0,0,4]]
         && sharedNormals upperFacets == [[0,0,-2]]
         && null (filter isInternal historicalAdjacency)
         && length historicalAdjacency == 8
         && External ([0,0,-1/2],[0,0,1]) `elem` historicalAdjacency
         && External ([0,0,1],[0,0,-1]) `elem` historicalAdjacency)
    , ("Exact reference: fractional simplex hand geometry", exactSkeleton3 simplexTerms == Right simplexExpected)
    , ("Corrected Yang/GLPK adapter: fractional simplex hand geometry", originalSkeleton3 simplexTerms == Right simplexExpected)
    , ("Independent LRS adapter: fractional simplex hand geometry", lrsSkeleton3 simplexTerms == Right simplexExpected)
    , ("Exact reference: unequal-height cells hand geometry", exactSkeleton3 unequalTerms == Right unequalExpected)
    , ("Corrected Yang/GLPK adapter: unequal-height bounded edge", originalSkeleton3 unequalTerms == Right unequalExpected)
    , ("Independent LRS adapter: unequal-height bounded edge", lrsSkeleton3 unequalTerms == Right unequalExpected)
    ]
  where
    historicalSimplex = getHyper [External (simplexVertex,d) | d <- simplexDirections]
    shared = [[0,0,1],[1,0,1],[0,1,1]]
    lowerFacets = facetEnumeration' ([0,0,0]:shared)
    upperFacets = facetEnumeration' ([0,0,3]:shared)
    sharedNormals fs = [h | (ps,h,_) <- fs,sort ps == sort shared]
    historicalAdjacency = graphHypersurface $ MS.fromList
        [([0,0,1],[(ps,h) | (ps,h,_) <- lowerFacets]),
         ([0,0,-1/2],[(ps,h) | (ps,h,_) <- upperFacets])]

simplexVertex :: Vertex
simplexVertex = [1/2,2/3,3/4]

simplexDirections :: [[Integer]]
simplexDirections = [[-6,-4,-3],[1,0,0],[0,1,0],[0,0,1]]

-- min(0, 2*x-1, 3*y-2, 4*z-3).
simplexTerms :: [Term3]
simplexTerms = [Term3 0 0 0 0,Term3 2 0 0 (-1),Term3 0 3 0 (-2),Term3 0 0 4 (-3)]

simplexExpected :: Skeleton3
simplexExpected = canonicalSkeleton3 [Ray3 simplexVertex d | d <- simplexDirections]

-- min(z, x+z, y+z, 1, 1+3*z). Along x=y=0 its bounded edge
-- is -1/2 <= z <= 1, with three unbounded edges at each endpoint.
unequalTerms :: [Term3]
unequalTerms = [Term3 0 0 1 0,Term3 1 0 1 0,Term3 0 1 1 0,Term3 0 0 0 1,Term3 0 0 3 1]

unequalExpected :: Skeleton3
unequalExpected = canonicalSkeleton3
    ([Segment3 [0,0,-1/2] [0,0,1]] ++
     [Ray3 [0,0,-1/2] d | d <- [[1,0,0],[0,1,0],[-2,-2,-1]]] ++
     [Ray3 [0,0,1] d | d <- [[1,0,0],[0,1,0],[-1,-1,1]]])
