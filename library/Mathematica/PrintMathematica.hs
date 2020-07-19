{-# LANGUAGE DataKinds, TypeFamilies, FlexibleContexts, FlexibleInstances, PolyKinds #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}

module Mathematica.PrintMathematica where

import System.IO 
import System.Directory
import System.Process
import Data.String
import Geometry.LRS
import Geometry.Facet
import Geometry.Polyhedral
import Geometry.Vertex 
import Polynomial.Hypersurface
import Polynomial.Monomial
import Polynomial.Prelude
import Polynomial.System
import Prelude
import Data.Ratio
import Data.Matrix hiding (trace)




showRat :: Rational -> String
showRat r = show nu ++ "/" ++ show de
        where 
            nu = numerator r 
            de = denominator r

showVertex :: Vertex -> String
showVertex vert = strVert 
         where 
            separator = \x  acc ->  x ++  ", " ++ acc 
            strVert = ( foldl1 separator . map (showRat) ) vert


printVerticesFile :: [Vertex] -> String -> IO ()
printVerticesFile vert file = do 
                             placeToSaveVert <- fmap (++ ("/Outputs/vert_" ++ file ++ ".txt" )) getCurrentDirectory
                             printVertices vert placeToSaveVert
                             line <- appendFile placeToSaveVert $ "\n"
                             return ()


printVertices ::  [Vertex] -> FilePath -> IO ()
printVertices [] path = return ()
printVertices [v] path = do 
                            line <- appendFile path $ showVertex v
                            printVertices [] path
printVertices (v:vs) path = do 
                            line <- appendFile path $ showVertex v
                            line <- appendFile path $ "; "
                            printVertices vs path

showRatList :: [Rational] -> String
showRatList rat  =  strVert 
            where 
                separator = \x  acc ->  x ++  ", " ++ acc 
                strVert = ( foldl1 separator . map (showRat) ) rat


printRows :: [Rational] -> Int -> Int -> FilePath -> IO ()
printRows [] _ _ _ = return ()
printRows mat 1 col path = do 
                    let row = take col mat 
                    let newMat = drop col mat
                    line <- appendFile path $ showRatList row
                    printRows newMat 0 col path
printRows mat rows col path = do 
                    let row = take col mat 
                    let newMat = drop col mat
                    line <- appendFile path $ showRatList row
                    line <- appendFile path $ "\n"
                    printRows newMat (rows-1) col path


printHyp :: Matrix Rational -> FilePath -> IO ()
printHyp mat file = do 
                       placeToSaveMat <- fmap (++ ("/Outputs/" ++ "hyp_" ++ file ++ ".txt" )) getCurrentDirectory
                       let cols = ncols mat 
                       let rows = nrows mat 
                       let flatMat = toList mat 
                       printRows flatMat rows cols placeToSaveMat
                       return ()

printB :: Matrix Rational -> FilePath -> IO ()
printB mat file = do 
                       placeToSaveMat <- fmap (++ ("/Outputs/" ++ "b_" ++ file ++ ".txt" )) getCurrentDirectory
                       let cols = ncols mat 
                       let rows = nrows mat 
                       let flatMat = toList mat 
                       printRows flatMat rows cols placeToSaveMat
                       return ()


printLF :: [[Vertex]] -> FilePath -> IO ()
printLF [] _ = return ()
printLF (v:vs) path = do 
                    printVertices v path
                    line <- appendFile path $ "\n"
                    printLF vs path 
            
lfToFile :: [[Vertex]] -> String -> IO ()
lfToFile vertices file = do 
                    placeToSave <- fmap (++ ("/Outputs/" ++ "LF_" ++ file ++ ".txt" )) getCurrentDirectory
                    printLF vertices placeToSave
                    return ()


printTrop ::  (IsMonomialOrder ord, Real k, Show k)  => Polynomial k ord n -> String -> IO ()
printTrop poly file = do
                    let points = expVecs poly
                    let hull = extremalVertices points
                    let facetEnumerated = facetEnumeration $ hull
                    let matsHyp = fromLists $ map (\(_,h,_) -> h) facetEnumerated
                    let bHyp = colFromList $ map (\(_,_,b) -> b) facetEnumerated
                    let lwFacets = lowerHull poly                    
                    printHyp matsHyp file
                    printB bHyp file
                    lfToFile  lwFacets file
                    printVerticesFile hull file
            
cleanFiles :: String -> IO ()
cleanFiles  file = callCommand string
        where
            string = "rm ~/Projects/Tropical/TGPackage/Code/tropical-geometry/Outputs/"++"*"++file++".txt"++" &"

runMathematica :: IO ()
runMathematica = callCommand "mathematica ~/Projects/Tropical/TGPackage/Code/tropical-geometry/MathematicaCode/TropicalGraphics.nb  &"