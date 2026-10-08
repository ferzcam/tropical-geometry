{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- JSON-lines adapter for the exact and tailored hull/curve implementations.
-- Input parsing happens before each algorithm timer; forcing happens inside it;
-- JSON serialization happens after it. This keeps lazy list traversal in the
-- measured algorithm time while excluding IPC and process startup.
module Main (main) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (unless)
import Data.Aeson
    ( FromJSON(..), Value, eitherDecodeStrict', encode, object, toJSON, withObject, (.:), (.:?), (.!=), (.=) )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Aeson.Types as AesonTypes
import Data.Ratio (denominator, numerator, (%))
import GHC.Clock (getMonotonicTimeNSec)
import Geometry.LRSHull (HullFacet)
import Geometry.TropicalCurve (EdgeGeometry(..), Term(..))
import Geometry.TropicalSlice (Term3(..))
import HullSkeleton3 (hullParityChecks3, lrsSkeleton3, originalSkeleton3)
import HistoricalRegression (historicalChecks)
import Methods
    ( CanonicalCurve(..), LegacySegment, exactCurve, lrsCurve, lrsHull
    , tailoredCurve, tailoredHull2, tailoredHull3
    , originalLegacyCurve, hullParityChecks2
    )
import Skeleton3 (Edge3(..), Skeleton3(..), exactSkeleton3, skeleton3Checks)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stdin, stdout, hFlush, hIsEOF)
import Text.Read (readMaybe)

data Request = Request
    { requestCaseId :: String
    , requestKind :: String
    , requestMethod :: String
    , requestPoints :: [[Rational]]
    , requestTerms :: [Term]
    , requestTerms3 :: [Term3]
    }

instance FromJSON Request where
    parseJSON = withObject "benchmark request" $ \o -> do
        caseId <- o .: "case_id"
        kind <- o .: "kind"
        method <- o .: "method"
        rawPoints <- o .:? "points" .!= []
        rawTerms <- o .:? "terms" .!= []
        points <- mapM (mapM parseRational) rawPoints
        terms <- if kind == "curve" then mapM parseTerm rawTerms else pure []
        terms3 <- if kind == "skeleton3" then mapM parseTerm3 rawTerms else pure []
        pure (Request caseId kind method points terms terms3)

parseTerm :: Value -> AesonTypes.Parser Term
parseTerm = withObject "term" $ \o -> do
    x <- o .: "x"
    y <- o .: "y"
    coefficient <- o .: "coefficient" >>= parseRational
    pure (Term x y coefficient)

parseTerm3 :: Value -> AesonTypes.Parser Term3
parseTerm3 = withObject "3D term" $ \o -> do
    x <- o .: "x"
    y <- o .: "y"
    z <- o .: "z"
    coefficient <- o .: "coefficient" >>= parseRational
    pure (Term3 x y z coefficient)

parseRational :: String -> AesonTypes.Parser Rational
parseRational raw = case break (== '/') raw of
    (whole, "") -> maybe (fail ("invalid rational: " ++ raw)) (pure . fromInteger) (readMaybe whole)
    (top, _:bottom) -> case (readMaybe top, readMaybe bottom) of
        (Just n, Just d) | d /= (0 :: Integer) -> pure (n % d)
        _ -> fail ("invalid rational: " ++ raw)

data Result
    = CurveResult CanonicalCurve
    | HullResult [HullFacet]
    | LegacyResult [LegacySegment]
    | Skeleton3Result Skeleton3

data Reply = Reply
    { replyCaseId :: String
    , replyMethod :: String
    , replyStatus :: String
    , replyError :: Maybe String
    , replyResult :: Maybe Value
    , replyWallNs :: Integer
    , replyCpuPs :: Integer
    }

main :: IO ()
main = do
    args <- getArgs
    if args == ["--self-test"] then runSelfTest else loop
  where
    loop = do
        done <- hIsEOF stdin
        unless done (BSC.getLine >>= processLine >> loop)

processLine :: BS.ByteString -> IO ()
processLine line = case eitherDecodeStrict' line of
    Left err -> emit (Reply "" "" "error" (Just err) Nothing 0 0)
    Right req -> do
        forced <- try (evaluate (forceRequest req))
        case forced of
            Left (err :: SomeException) -> emit (requestError req (displayException err))
            Right () -> do
                wallStart <- getMonotonicTimeNSec
                cpuStart <- getCPUTime
                outcome <- try (do
                    result <- runMethod req
                    case result of
                        Left message -> pure (Left message)
                        Right answer -> evaluate (forceResult answer) >> pure (Right answer))
                    :: IO (Either SomeException (Either String Result))
                cpuEnd <- getCPUTime
                wallEnd <- getMonotonicTimeNSec
                let wall = toInteger (wallEnd - wallStart)
                    cpu = cpuEnd - cpuStart
                case outcome of
                    Left (err :: SomeException) -> emit (Reply
                        (requestCaseId req) (requestMethod req) "error"
                        (Just (displayException err)) Nothing wall cpu)
                    Right (Left message) -> emit (Reply
                        (requestCaseId req) (requestMethod req) (classifyFailure message)
                        (Just message) Nothing wall cpu)
                    Right (Right answer) -> emit (Reply
                        (requestCaseId req) (requestMethod req) "ok" Nothing
                        (Just (encodeResult answer)) wall cpu)

requestError :: Request -> String -> Reply
requestError req message = Reply
    (requestCaseId req) (requestMethod req) "error" (Just message) Nothing 0 0

emit :: Reply -> IO ()
emit reply = do
    BL8.putStrLn (encode (replyValue reply))
    hFlush stdout

replyValue :: Reply -> Value
replyValue reply = object $
    [ "case_id" .= replyCaseId reply
    , "method" .= replyMethod reply
    , "status" .= replyStatus reply
    , "wall_ns" .= replyWallNs reply
    , "cpu_ps" .= replyCpuPs reply
    ] ++ maybe [] (\x -> ["error" .= x]) (replyError reply)
      ++ maybe [] (\x -> ["result" .= x]) (replyResult reply)

runMethod :: Request -> IO (Either String Result)
runMethod req = pure $ case (requestKind req, requestMethod req) of
    ("curve", "exact") -> CurveResult <$> exactCurve (requestTerms req)
    ("curve", "tailored") -> CurveResult <$> tailoredCurve (requestTerms req)
    ("curve", "lrs") -> CurveResult <$> lrsCurve (requestTerms req)
    ("curve", "legacy") -> LegacyResult <$> originalLegacyCurve (requestTerms req)
    ("skeleton3", "exact") -> Skeleton3Result <$> exactSkeleton3 (requestTerms3 req)
    ("skeleton3", "original") -> Skeleton3Result <$> originalSkeleton3 (requestTerms3 req)
    ("skeleton3", "lrs") -> Skeleton3Result <$> lrsSkeleton3 (requestTerms3 req)
    ("hull2", "tailored") -> HullResult <$> tailoredHull2 (requestPoints req)
    ("hull3", "tailored") -> HullResult <$> tailoredHull3 (requestPoints req)
    ("hull2", "lrs") -> HullResult <$> lrsHull (requestPoints req)
    ("hull3", "lrs") -> HullResult <$> lrsHull (requestPoints req)
    _ -> Left "unsupported kind/method combination"

-- Only documented input-domain exclusions count as unsupported. Any other
-- Left result exposes an algorithm/adapter failure and blocks timing.
classifyFailure :: String -> String
classifyFailure message
    | any (`contains` message) unsupportedMarkers = "unsupported"
    | otherwise = "error"
  where
    unsupportedMarkers =
        [ "Affine exponent support is unsupported"
        , "At least one finite polynomial term is required"
        , "At most 64 polynomial terms are supported"
        , "A nonempty polynomial is required"
        , "A nonempty point set is required"
        , "full affine dimension"
        , "ambient dimension must be"
        , "requires integral"
        , "require integral"
        , "requires integral dual vertices"
        , "outside the legacy Int range"
        , "cannot represent full lines"
        , "affine rank three"
        , "nonempty finite polynomial required"
        , "at most 32 terms are supported"
        , "nonempty three-variable polynomial is required"
        , "requires integral coefficients"
        , "requires coordinates within the legacy Int range"
        , "full-dimensional exponent support"
        , "requires full-dimensional exponent support"
        , "at most 32 terms"
        ]
    contains needle haystack = any (needle `prefixOf`) (tails haystack)
    prefixOf [] _ = True
    prefixOf _ [] = False
    prefixOf (a:as) (b:bs) = a == b && prefixOf as bs
    tails [] = [[]]
    tails s@(_:rest) = s : tails rest

forceRequest :: Request -> ()
forceRequest req = forceString (requestCaseId req)
    `seq` forceString (requestKind req)
    `seq` forceString (requestMethod req)
    `seq` forcePoints (requestPoints req)
    `seq` forceTerms (requestTerms req)
    `seq` forceTerms3 (requestTerms3 req)

forceString :: String -> ()
forceString = foldr (\c rest -> c `seq` rest) ()

forcePoints :: [[Rational]] -> ()
forcePoints = foldr (\point rest -> forceRationals point `seq` rest) ()

forceRationals :: [Rational] -> ()
forceRationals = foldr (\value rest -> forceRational value `seq` rest) ()

forceRational :: Rational -> ()
forceRational value = numerator value `seq` denominator value `seq` ()

forceTerms :: [Term] -> ()
forceTerms = foldr (\(Term x y c) rest -> x `seq` y `seq` forceRational c `seq` rest) ()

forceTerms3 :: [Term3] -> ()
forceTerms3 = foldr (\(Term3 x y z c) rest -> x `seq` y `seq` z `seq` forceRational c `seq` rest) ()

forceResult :: Result -> ()
forceResult (HullResult facets) = forceFacets facets
forceResult (CurveResult (CanonicalCurve (vertices, edges, cells))) =
    forceCurvePoints vertices `seq` forceEdges edges `seq` forceCells cells
forceResult (LegacyResult segments) = forceLegacySegments segments
forceResult (Skeleton3Result skeleton) =
    forcePoints (vertices3 skeleton) `seq` forceEdges3 (edges3 skeleton)

forceFacets :: [HullFacet] -> ()
forceFacets = foldr (\(normal,bound) rest -> forceRationals (normal ++ [bound]) `seq` rest) ()

forceCurvePoints :: [(Rational,Rational)] -> ()
forceCurvePoints = foldr (\(x,y) rest -> forceRational x `seq` forceRational y `seq` rest) ()

forceEdges :: [(EdgeGeometry,Integer)] -> ()
forceEdges = foldr (\(geometry,weight) rest -> forceGeometry geometry `seq` weight `seq` rest) ()

forceGeometry :: EdgeGeometry -> ()
forceGeometry (Segment a b) = forceCurvePoints [a,b]
forceGeometry (Ray p (dx,dy)) = forceCurvePoints [p] `seq` dx `seq` dy `seq` ()
forceGeometry (Line p (dx,dy)) = forceCurvePoints [p] `seq` dx `seq` dy `seq` ()

forceCells :: [[(Integer,Integer)]] -> ()
forceCells = foldr (\cell rest -> foldr (\(x,y) tailValue -> x `seq` y `seq` tailValue) () cell `seq` rest) ()

forceLegacySegments :: [LegacySegment] -> ()
forceLegacySegments = foldr (\((x,y),(u,v)) rest -> x `seq` y `seq` u `seq` v `seq` rest) ()

forceEdges3 :: [Edge3] -> ()
forceEdges3 = foldr (\edge rest -> forceEdge3 edge `seq` rest) ()

forceEdge3 :: Edge3 -> ()
forceEdge3 (Segment3 a b) = forceRationals a `seq` forceRationals b
forceEdge3 (Ray3 p direction) = forceRationals p `seq` forceIntegers direction
forceEdge3 (Line3 p direction) = forceRationals p `seq` forceIntegers direction

forceIntegers :: [Integer] -> ()
forceIntegers = foldr (\value rest -> value `seq` rest) ()

encodeResult :: Result -> Value
encodeResult (HullResult facets) = object ["facets" .= map encodeFacet facets]
encodeResult (LegacyResult segments) = object
    [ "legacySegments" .= map encodeLegacySegment segments ]
encodeResult (Skeleton3Result skeleton) = object
    [ "vertices" .= map encodeRationalVector (vertices3 skeleton)
    , "edges" .= map encodeEdge3 (edges3 skeleton)
    ]
encodeResult (CurveResult (CanonicalCurve (vertices, edges, cells))) = object
    [ "vertices" .= map encodePoint vertices
    , "edges" .= map encodeEdge edges
    , "cells" .= map (map encodeExponent) cells
    ]

encodeFacet :: HullFacet -> Value
encodeFacet (normal,bound) = object
    [ "normal" .= map rationalText normal
    , "bound" .= rationalText bound
    ]

encodePoint :: (Rational,Rational) -> Value
encodePoint (x,y) = toJSON [rationalText x,rationalText y]

encodeExponent :: (Integer,Integer) -> Value
encodeExponent (x,y) = toJSON [x,y]

encodeLegacySegment :: LegacySegment -> Value
encodeLegacySegment ((x,y),(u,v)) = toJSON [[x,y],[u,v]]

encodeRationalVector :: [Rational] -> Value
encodeRationalVector = toJSON . map rationalText

encodeEdge3 :: Edge3 -> Value
encodeEdge3 edge = case edge of
    Segment3 start end -> object
        [ "kind" .= ("segment" :: String), "start" .= encodeRationalVector start
        , "end" .= encodeRationalVector end ]
    Ray3 start direction -> object
        [ "kind" .= ("ray" :: String), "start" .= encodeRationalVector start
        , "direction" .= direction ]
    Line3 start direction -> object
        [ "kind" .= ("line" :: String), "start" .= encodeRationalVector start
        , "direction" .= direction ]

encodeEdge :: (EdgeGeometry,Integer) -> Value
encodeEdge (geometry,weight) = case geometry of
    Segment start end -> object
        [ "kind" .= ("segment" :: String), "start" .= encodePoint start
        , "end" .= encodePoint end, "weight" .= weight ]
    Ray start direction -> object
        [ "kind" .= ("ray" :: String), "start" .= encodePoint start
        , "direction" .= encodeExponent direction, "weight" .= weight ]
    Line start direction -> object
        [ "kind" .= ("line" :: String), "start" .= encodePoint start
        , "direction" .= encodeExponent direction, "weight" .= weight ]

rationalText :: Rational -> String
rationalText value
    | denominator value == 1 = show (numerator value)
    | otherwise = show (numerator value) ++ "/" ++ show (denominator value)

runSelfTest :: IO ()
runSelfTest = do
    let checks = skeleton3Checks ++ historicalChecks ++ hullParityChecks2 ++ hullParityChecks3
    mapM_ (\(name,passed) -> putStrLn (name ++ ": " ++ if passed then "PASS" else "FAIL")) checks
    if all snd checks
        then putStrLn ("Skeleton3 self-tests passed (" ++ show (length checks) ++ " checks).")
        else exitFailure
