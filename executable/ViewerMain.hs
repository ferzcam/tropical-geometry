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
import Geometry.TropicalSlice
import Text.Read (readMaybe)

data Request = CurveRequest [Term] | SliceRequest [Term3] Rational

instance FromJSON Request where
  parseJSON = withObject "request" $ \o -> do
    if KeyMap.member "height" o then do
      heightText <- o .: "height"
      h <- case parseRational heightText of
        Nothing -> fail "Height must be an integer or fraction string, with at most 18 digits per part."
        Just q -> pure q
      ts <- o .: "terms" >>= mapM parseTerm3
      checkCount ts
      pure (SliceRequest ts h)
    else do
      ts <- o .: "terms" >>= mapM parseTerm
      checkCount ts
      pure (CurveRequest ts)

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

curveJSON :: Curve -> Value
curveJSON = object . curveFields

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

sliceJSON :: SliceResult -> Value
sliceJSON result = object $
  curveFields (sliceCurve result) ++
  [ "height" .= rationalText (sliceHeight result)
  , "sourceTerms" .= zipWith sourceTermJSON [0..] (sliceSourceTerms result)
  , "regions" .= map regionJSON (sliceRegions result)
  ]

respond :: B.ByteString -> B.ByteString
respond input = encode $ case eitherDecode input of
  Left err -> object ["error" .= err]
  Right (CurveRequest ts) -> either (\err -> object ["error" .= err]) curveJSON (tropicalCurve ts)
  Right (SliceRequest ts height) -> either (\err -> object ["error" .= err]) sliceJSON (tropicalSlice ts height)

main :: IO ()
main = do
  input <- B.getContents
  if B.length (B.take 32769 input) > 32768 then B.putStrLn (encode (object ["error" .= ("Request exceeds 32768 bytes." :: String)])) else do
    let output = respond input
    result <- try (evaluate (B.length output)) :: IO (Either SomeException Int64)
    case result of
      Left _ -> B.putStrLn (encode (object ["error" .= ("Geometry computation failed." :: String)]))
      Right _ -> B.putStrLn output
