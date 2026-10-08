{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (SomeException, evaluate, try)
import Data.Aeson
import Data.Aeson.Types (Parser, Pair)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isDigit)
import Data.Int (Int64)
import Data.Ratio ((%), numerator, denominator)
import qualified Data.Text as T
import Geometry.TropicalCurve
import Geometry.TropicalGraph3
import Geometry.TropicalHull2 (hullTropicalCurve)
import Geometry.TropicalHull3 (hullGraph3)
import Geometry.TropicalSkeleton3 (Edge3(..))
import Geometry.TropicalSlice
import Geometry.TropicalSlice4
import Text.Read (readMaybe)

-- | Every request names its algorithm explicitly. The direct solver is the
-- default for curves and 3D graphs when "method" is absent, so the original
-- request contract is unchanged; slices always use the direct solver.
data Method = Direct | Hull | LRS deriving (Eq, Show)

data Request
    = CurveRequest Method [Term]
    | SliceRequest [Term3] Rational
    | Graph3Request Method [Term3]
    | Slice4Request [Term4] Rational

instance FromJSON Request where
  parseJSON = withObject "request" $ \o -> do
    kind <- o .:? "kind" :: Parser (Maybe String)
    case kind of
      Just "graph3" -> do
        method <- parseMethod o
        ts <- o .: "terms" >>= mapM parseTerm3
        checkCount ts
        pure (Graph3Request method ts)
      Just other -> fail ("Unknown request kind: " ++ other ++ ".")
      Nothing
        | KeyMap.member "height" o -> do
            heightText <- o .: "height"
            h <- case parseRational heightText of
              Nothing -> fail "Height must be an integer or fraction string, with at most 18 digits per part."
              Just q -> pure q
            values <- o .: "terms"
            let isFour = case values of
                  (Object term : _) -> KeyMap.member "w" term
                  _ -> False
            if isFour then do
              ts <- mapM parseTerm4 values
              checkCount ts
              pure (Slice4Request ts h)
            else do
              ts <- mapM parseTerm3 values
              checkCount ts
              pure (SliceRequest ts h)
        | otherwise -> do
            method <- parseMethod o
            ts <- o .: "terms" >>= mapM parseTerm
            checkCount ts
            pure (CurveRequest method ts)

parseMethod :: Object -> Parser Method
parseMethod o = do
  name <- o .:? "method" :: Parser (Maybe String)
  case name of
    Nothing -> pure Direct
    Just "direct" -> pure Direct
    Just "hull" -> pure Hull
    Just "lrs" -> pure LRS
    Just other -> fail ("Unknown method: " ++ other ++ ". Use direct, hull, or lrs.")

methodName :: Method -> String
methodName Direct = "direct"
methodName Hull = "hull"
methodName LRS = "lrs"

checkCount :: [a] -> Parser ()
checkCount ts
  | null ts || length ts > 32 = fail "Provide 1 to 32 terms."
  | otherwise = pure ()

parseTerm :: Value -> Parser Term
parseTerm = withObject "term" $ \o -> do
  x <- o .: "x"
  y <- o .: "y"
  c <- o .: "coefficient"
  checkExponents [x,y]
  coefficient <- parseCoefficient c
  pure (Term x y coefficient)

parseTerm3 :: Value -> Parser Term3
parseTerm3 = withObject "term" $ \o -> do
  x <- o .: "x"
  y <- o .: "y"
  z <- o .: "z"
  c <- o .: "coefficient"
  checkExponents [x,y,z]
  coefficient <- parseCoefficient c
  pure (Term3 x y z coefficient)

parseTerm4 :: Value -> Parser Term4
parseTerm4 = withObject "term" $ \o -> do
  x <- o .: "x"
  y <- o .: "y"
  z <- o .: "z"
  w <- o .: "w"
  c <- o .: "coefficient"
  checkExponents [x,y,z,w]
  coefficient <- parseCoefficient c
  pure (Term4 x y z w coefficient)

checkExponents :: [Integer] -> Parser ()
checkExponents exponents
  | all (\x -> abs x <= 100) exponents = pure ()
  | otherwise = fail "Exponents must be within -100 and 100."

parseCoefficient :: String -> Parser Rational
parseCoefficient text = case parseRational text of
  Nothing -> fail "Coefficient must be an integer or fraction string, with at most 18 digits per part."
  Just q -> pure q

parseRational :: String -> Maybe Rational
parseRational s = case break (== '/') s of
  (n, "") -> (% 1) <$> signed n
  (n, '/':d) | not (null d) && head d /= '0' -> do
    a <- signed n
    b <- unsigned d
    if b > 0 then Just (a % b) else Nothing
  _ -> Nothing
  where
    unsigned x | not (null x) && length x <= 18 && all (\c -> isDigit c && c <= '9') x = readMaybe x
               | otherwise = Nothing
    signed ('-':x) = negate <$> unsigned x
    signed x = unsigned x

rationalText :: Rational -> String
rationalText x = show (numerator x) ++ if denominator x == 1 then "" else "/" ++ show (denominator x)

pointJSON :: Point -> Value
pointJSON (x,y) = toJSON [rationalText x, rationalText y]

termJSON :: Term -> Value
termJSON t = object ["x" .= termX t, "y" .= termY t, "coefficient" .= rationalText (termCoefficient t)]

vertexJSON :: CurveVertex -> Value
vertexJSON v = object ["id" .= vertexId v, "point" .= pointJSON (vertexPoint v), "terms" .= vertexTerms v]

edgeJSON :: CurveEdge -> Value
edgeJSON e = object $ ["id" .= edgeId e, "terms" .= edgeTerms e, "weight" .= show (edgeWeight e), "dual" .= [fst (edgeDual e), snd (edgeDual e)]] ++ shape (edgeGeometry e)
  where
    shape (Segment a b) = ["kind" .= ("segment" :: T.Text), "start" .= pointJSON a, "end" .= pointJSON b]
    shape (Ray a d) = ["kind" .= ("ray" :: T.Text), "start" .= pointJSON a, "direction" .= direction d]
    shape (Line a d) = ["kind" .= ("line" :: T.Text), "start" .= pointJSON a, "direction" .= direction d]
    direction (x,y) = [show x, show y]

cellJSON :: SubdivisionCell -> Value
cellJSON c = object ["id" .= cellId c, "vertex" .= cellVertexId c, "terms" .= cellTerms c, "boundary" .= cellBoundary c]

curveFields :: Curve -> [Pair]
curveFields c = ["terms" .= map termJSON (curveTerms c), "vertices" .= map vertexJSON (curveVertices c), "edges" .= map edgeJSON (curveEdges c), "cells" .= map cellJSON (curveCells c)]

curveJSON :: Method -> Curve -> Value
curveJSON method c = object (("method" .= methodName method) : curveFields c)

sourceTermJSON :: Int -> Term3 -> Value
sourceTermJSON i t = object
  [ "id" .= i, "x" .= term3X t, "y" .= term3Y t, "z" .= term3Z t
  , "coefficient" .= rationalText (term3Coefficient t)
  ]

regionJSON :: SliceRegion -> Value
regionJSON region = object
  [ "terms" .= regionTerms region
  , "inequalities" .= map inequalityJSON (regionInequalities region)
  ]
  where
    inequalityJSON inequality = object
      [ "a" .= show (inequalityA inequality)
      , "b" .= show (inequalityB inequality)
      , "bound" .= rationalText (inequalityBound inequality)
      ]

term4JSON :: Term4 -> Value
term4JSON t = object
  [ "x" .= term4X t, "y" .= term4Y t, "z" .= term4Z t, "w" .= term4W t
  , "coefficient" .= rationalText (term4Coefficient t)
  ]
patch4JSON :: Slice3Patch -> Value
patch4JSON patch = object
  [ "terms" .= let (a,b) = patchTerms patch in [a,b]
  , "equality" .= planeJSON (patchEquality patch)
  , "inequalities" .= map planeJSON (patchInequalities patch)
  ]
  where
    planeJSON (a,b,c,bound) = object
      [ "a" .= a, "b" .= b, "c" .= c, "bound" .= rationalText bound ]
slice4JSON :: Slice4Result -> Value
slice4JSON result = object
  [ "height" .= rationalText (slice4Height result)
  , "sourceTerms" .= map term4JSON (slice4SourceTerms result)
  , "patches" .= map patch4JSON (slice4Patches result)
  ]

sliceJSON :: SliceResult -> Value
sliceJSON result = object $
  curveFields (sliceCurve result) ++
  [ "height" .= rationalText (sliceHeight result)
  , "sourceTerms" .= zipWith sourceTermJSON [0..] (sliceSourceTerms result)
  , "regions" .= map regionJSON (sliceRegions result)
  ]

point3JSON :: [Rational] -> Value
point3JSON = toJSON . map rationalText

vertex3JSON :: GraphVertex3 -> Value
vertex3JSON v = object
  [ "id" .= vertex3Id v, "point" .= point3JSON (vertex3Point v)
  , "terms" .= vertex3Terms v, "cell" .= vertex3Cell v ]

edge3JSON :: GraphEdge3 -> Value
edge3JSON e = object $
  [ "id" .= edge3Id e, "terms" .= edge3Terms e, "face" .= edge3Face e
  , "vertices" .= edge3Vertices e ] ++ shape (edge3Geometry e)
  where
    shape (Segment3 a b) = ["kind" .= ("segment" :: T.Text), "start" .= point3JSON a, "end" .= point3JSON b]
    shape (Ray3 a d) = ["kind" .= ("ray" :: T.Text), "start" .= point3JSON a, "direction" .= map show d]
    shape (Line3 a d) = ["kind" .= ("line" :: T.Text), "start" .= point3JSON a, "direction" .= map show d]

cell3JSON :: SubdivisionCell3 -> Value
cell3JSON c = object
  [ "id" .= cell3Id c, "vertex" .= cell3Vertex c, "terms" .= cell3Terms c
  , "faces" .= cell3Faces c ]

face3JSON :: SubdivisionFace3 -> Value
face3JSON f = object
  [ "id" .= face3Id f, "terms" .= face3Terms f, "boundary" .= face3Boundary f
  , "cells" .= face3Cells f, "edge" .= face3Edge f ]

-- The "contract" field states what this geometry is: the graph one-skeleton
-- (vertices and edges) with its dual cells and faces, not the full surface.
graph3JSON :: Method -> Graph3 -> Value
graph3JSON method g = object
  [ "kind" .= ("graph3" :: T.Text)
  , "contract" .= ("one-skeleton" :: T.Text)
  , "method" .= methodName method
  , "terms" .= zipWith sourceTermJSON [0..] (graph3Terms g)
  , "vertices" .= map vertex3JSON (graph3Vertices g)
  , "edges" .= map edge3JSON (graph3Edges g)
  , "cells" .= map cell3JSON (graph3Cells g)
  , "faces" .= map face3JSON (graph3Faces g)
  ]

curveRoute :: Method -> [Term] -> Either String Curve
curveRoute Direct = tropicalCurve
curveRoute Hull = hullTropicalCurve
curveRoute LRS = lrsTropicalCurve

graph3Route :: Method -> [Term3] -> Either String Graph3
graph3Route Direct = exactGraph3
graph3Route Hull = hullGraph3
graph3Route LRS = lrsGraph3

respond :: B.ByteString -> B.ByteString
respond input = encode $ case eitherDecode input of
  Left err -> object ["error" .= err]
  Right (CurveRequest method ts) -> either failure (curveJSON method) (curveRoute method ts)
  Right (SliceRequest ts height) -> either failure sliceJSON (tropicalSlice ts height)
  Right (Graph3Request method ts) -> either failure (graph3JSON method) (graph3Route method ts)
  Right (Slice4Request ts height) -> either failure slice4JSON (tropicalSlice4 ts height)
  where
    failure err = object ["error" .= err]

main :: IO ()
main = do
  input <- B.getContents
  if B.length (B.take 32769 input) > 32768 then B.putStrLn (encode (object ["error" .= ("Request exceeds 32768 bytes." :: String)])) else do
    let output = respond input
    result <- try (evaluate (B.length output)) :: IO (Either SomeException Int64)
    case result of
      Left _ -> B.putStrLn (encode (object ["error" .= ("Geometry computation failed." :: String)]))
      Right _ -> B.putStrLn output
