{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (SomeException, evaluate, try)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isDigit)
import Data.Int (Int64)
import Data.Ratio ((%), numerator, denominator)
import qualified Data.Text as T
import Geometry.TropicalCurve
import Text.Read (readMaybe)

newtype Request = Request [Term]

instance FromJSON Request where
  parseJSON = withObject "request" $ \o -> do
    ts <- o .: "terms" >>= mapM parseTerm
    if null ts || length ts > 32 then fail "Provide 1 to 32 terms." else pure (Request ts)

parseTerm :: Value -> Parser Term
parseTerm = withObject "term" $ \o -> do
  x <- o .: "x"
  y <- o .: "y"
  c <- o .: "coefficient"
  if abs x > 100 || abs y > 100 then fail "Exponents must be within -100 and 100." else pure ()
  case parseRational c of
    Nothing -> fail "Coefficient must be an integer or fraction string, with at most 18 digits per part."
    Just q -> pure (Term x y q)

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

curveJSON :: Curve -> Value
curveJSON c = object ["terms" .= map termJSON (curveTerms c), "vertices" .= map vertexJSON (curveVertices c), "edges" .= map edgeJSON (curveEdges c), "cells" .= map cellJSON (curveCells c)]

respond :: B.ByteString -> B.ByteString
respond input = encode $ case eitherDecode input of
  Left err -> object ["error" .= err]
  Right (Request ts) -> either (\err -> object ["error" .= err]) curveJSON (tropicalCurve ts)

main :: IO ()
main = do
  input <- B.getContents
  if B.length (B.take 32769 input) > 32768 then B.putStrLn (encode (object ["error" .= ("Request exceeds 32768 bytes." :: String)])) else do
    let output = respond input
    result <- try (evaluate (B.length output)) :: IO (Either SomeException Int64)
    case result of
      Left _ -> B.putStrLn (encode (object ["error" .= ("Geometry computation failed." :: String)]))
      Right _ -> B.putStrLn output
