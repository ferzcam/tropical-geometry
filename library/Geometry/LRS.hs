{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Geometry.LRS where

import Geometry.Vertex

import Data.Matrix hiding (trace)
import qualified Data.Vector as V
import Data.Maybe
import Data.List
import Util
import Control.Lens 
import Debug.Trace


type Row = Matrix Rational
type Col = Matrix Rational

data Dictionary = Dict {_idxs :: [Int], _dict :: Matrix Rational} deriving (Show)

makeLenses ''Dictionary

numRows :: Dictionary -> Int
numRows dictionary = nrows $ dictionary^.dict

numCols :: Dictionary -> Int
numCols dictionary = ncols $ dictionary^.dict

(|*|) = multStrassen


colFromList :: [a] -> Matrix a
colFromList = colVector . V.fromList

rowFromList :: [a] -> Matrix a
rowFromList = rowVector . V.fromList

-- | Wrapper of submatrix function. This function works for indices starting from 0 rather than 1 in the submatrix function
submatrix' :: 
    (Int, Int) ->   -- ^ Rows indices
    (Int, Int) ->   -- ^ Cols indices
    Matrix a ->
    Matrix a
submatrix' (ro,rk) (co,ck) = submatrix (ro+1) (rk+1) (co+1) (ck+1) 


-- | Lens for columns
colAt :: Int -> Lens' (Matrix Rational) Col
colAt j = lens (getCol' j) (\m c -> setCol' j c m)

getCol' :: Int -> Matrix a -> Matrix a
getCol' idx = colVector . (getCol (idx+1))

setCol' :: Int -> Col -> Matrix Rational -> Matrix Rational
setCol' idx col mat 
    | idx == 0 = col <|> right
    | idx == (ncols mat) - 1 = left <|> col
    | otherwise = left <|> col <|> right
    where
        left = submatrix' (0,(nrows mat)-1) (0,idx-1) mat
        right = submatrix' (0,(nrows mat)-1) (idx+1, (ncols mat)-1) mat

-- | Lens for rows
rowAt :: Int -> Lens' (Matrix Rational) Row
rowAt i = lens (getRow' i) (\m r -> setRow' i r m)

getRow' :: Int -> Matrix a -> Matrix a
getRow' idx = rowVector . (getRow (idx+1))

setRow' :: Int -> Col -> Matrix Rational -> Matrix Rational
setRow' idx row mat
    | idx == 0 = row <-> down
    | idx == (nrows mat) - 1 = up <-> row
    | otherwise = up <-> row <-> down
   
    where
        up = submatrix' (0,idx-1) (0,(ncols mat)-1)  mat
        down = submatrix' (idx+1, (nrows mat)-1) (0,(ncols mat)-1) mat

-- | Lens for accessing elements

elemAt :: (Int, Int) -> Lens' (Matrix a) a
elemAt (i, j) = lens (getElem' (i,j)) (\m x -> setElem' x (i,j) m) 

getElem' :: (Int, Int) -> Matrix a -> a
getElem' (row, col) = getElem (row+1) (col+1)

setElem' :: a -> (Int, Int) -> Matrix a -> Matrix a
setElem' elem (row, col) mat
    | row < 0 || col < 0 || row >= nrows mat || col >= ncols mat = error ("setElem': Trying to set at (" ++ show row ++ "," ++ show col ++ ") in a " ++ show (nrows mat) ++ "x" ++ show (ncols mat) ++ " matrix.")
    | otherwise = setElem elem (row+1, col+1) mat

mapCol' :: (Int -> a -> a) -> Int -> Matrix a -> Matrix a
mapCol' f col = mapCol f (col+1)

mapRow' :: (Int -> a -> a) -> Int -> Matrix a -> Matrix a
mapRow' f col = mapRow f (col+1)




{- 
    Dictionary form

                p21     p31
    invA_b
                p22     p32

 -}




getDictionary :: Matrix Rational -> Col -> Dictionary
getDictionary _A b = Dict [0..rows+1+cols] (identity (rows+1)  <|> (p21 <-> p22)  <|> (p31 <-> p32))
    where
        rows = nrows _A
        cols = ncols _A
        slack = identity rows
        topRow = rowFromList $ (replicate (cols+1) 1) ++ (replicate rows 0)
        leftColumn = colFromList (replicate rows 0)
        newA =  _A <|> slack --matrix form
        _A_b = submatrix' (0, rows-1) (0, rows-1) newA -- basis
        _A_n = submatrix' (0, rows-1) (rows, rows+cols-1) newA -- cobasis
        c_b = submatrix' (0, 0) (1, rows) topRow -- basic part of top row
        c_n = submatrix' (0, 0) (rows+1, rows+cols) topRow -- cobasic part of top row
        -- parts of the dictionary 
        Right invA_b = inverse _A_b
        p21 = c_n - (c_b |*| invA_b |*| _A_n)
        p22 = invA_b |*| _A_n
        p31 = negate $ c_b |*| invA_b |*| b
        p32 = invA_b |*| b
        



simplex :: Dictionary -> Dictionary
simplex dictionary
    | idxsToPivot == Nothing = dictionary
    | otherwise = simplex $ uncurry pivot (fromJust idxsToPivot) dictionary
    where
        idxsToPivot = selectPivot dictionary
        dictMatrix = dictionary^.dict
    --     rows = nrows dictMatrix
    --     cols = ncols dictMatrix
    --     dim = cols - rows - 1
    --     cobasic = (head.toLists.(submatrix' (0, 0) (rows, rows+dim-1))) dictMatrix
    --     idxEntering = enteringVariable cobasic rows
    --    -- invA_b = submatrix' (0, rows-1) (1, rows-1) dictMatrix
    --     idxLeaving = lexMinRatio dictionary (fromJust idxEntering)

enteringVariable :: (Num a, Ord a) => [a] -> Int -> Maybe Int
enteringVariable cobasic dimB = fmap ((+) (dimB)) $ findIndex (<0) cobasic

lexMinRatio :: Dictionary -> Int -> Int
lexMinRatio dictionary s
    | null indexed_s = 0
    | otherwise = fst $ indexed_s !! (fromJust $ elemIndex (minimum ratios) ratios)
    where
        dictMatrix = dictionary^.dict
        rows = nrows dictMatrix
        cols = ncols dictMatrix
        dim = cols - rows - 1
        _D = (dictMatrix ^. colAt (cols-1)) <|> submatrix' (0, rows-1) (1, rows-1) dictMatrix
        s_col = dictMatrix ^. colAt s
        slice_s = (concat . toLists . (submatrix' (dim+1, rows-1) (0,0)))  s_col
        indexed_s = filter (snd.(fmap (>0))) $ zip [(dim+1)..] slice_s
        sub_D = map (head .toLists . (\i -> _D ^. rowAt i) . fst) indexed_s -- takes rows with indices as in indexed_s
        ratios = safeZipWith (map $) (map ((flip (/)) . snd) indexed_s) sub_D


selectPivot :: Dictionary -> Maybe (Int, Int)
selectPivot dictionary
    | idxEntering == Nothing = Nothing
    | otherwise = trace ("PIVOT>> " ++ show (idxLeaving, (fromJust idxEntering))) Just $ (,) idxLeaving (fromJust idxEntering)
    where
        dictMatrix = dictionary^.dict
        rows = nrows dictMatrix
        cols = ncols dictMatrix
        dim = cols - rows - 1
        cobasic = (head.toLists.(submatrix' (0, 0) (rows, rows+dim-1))) dictMatrix
        idxEntering = enteringVariable cobasic rows
        idxLeaving = lexMinRatio dictionary (fromJust idxEntering)


pivot :: Int -> Int -> Dictionary -> Dictionary
pivot r s dictionary = Dict ( dictionary^.idxs & ((element r .~ _N_s) . (element s .~ _B_r))) (_E |*| dictMatrix)
    where
        _B_r = (dictionary^.idxs) !! r
        _N_s = (dictionary^.idxs) !! s
        dictMatrix = dictionary^.dict
        col_s = dictMatrix ^. colAt s
        a_t = col_s ^. elemAt (r, 0)
        eta = (mapCol' (\_ x -> -x/a_t) 0 col_s) & elemAt (r,0) .~ (1/a_t)
        _E = (identity (nrows dictMatrix)) & colAt r .~ eta


lexmin :: Dictionary -> Int -> Bool
lexmin dictionary _
    | idxEntering == Nothing = True
    | otherwise = not $ r>s && (dictMatrix ^. elemAt (idxLeaving,rows-1)) == 0 && (s_col ^. elemAt (idxLeaving,0)) /= 0
    where
        dictMatrix = dictionary^.dict
        rows = nrows dictMatrix
        cols = ncols dictMatrix
        dim = cols - rows - 1
        cobasic = (head.toLists.(submatrix' (0, 0) (rows, rows+dim-1))) dictMatrix 
        idxEntering = enteringVariable cobasic rows
        idxLeaving = lexMinRatio dictionary (fromJust idxEntering)
        r = (dictionary ^. idxs) !! idxLeaving
        s = (dictionary ^. idxs) !! (fromJust idxEntering)
        s_col = dictMatrix ^. colAt (fromJust idxEntering)


reverseRS :: Dictionary -> Int -> Maybe Int
reverseRS dictionary v 
    | conditions == False = Nothing
    | conditions == True = Just u
    where
        rows = numRows dictionary
        cols = numCols dictionary
        dictMatrix = dictionary^.dict
        v_col = dictMatrix ^. colAt v
        w_row_0 = dictMatrix ^. rowAt 0
        w_row_u = mapRow' (\_ x -> (v_col ^. elemAt (0,0))/(v_col ^. elemAt (u,0)) * x) 0 $ dictMatrix ^. rowAt u
        diff_ws = (head.toLists) $ w_row_0 - w_row_u
        lastCondition = all (>=0) [(diff_ws!!j)| j <- ( map ((dictionary^.idxs)!!) [rows..cols-2]), (dictionary^.idxs)!!j < (dictionary^.idxs)!!u]
        u = lexMinRatio dictionary v
        conditions = (w_row_0 ^. elemAt (0,v)) > 0 && (dictionary^.idxs)!!u /= 0 && lastCondition 


sortDictionary :: Dictionary -> Dictionary
sortDictionary (Dict idxs dictMatrix) = Dict (sort idxs) ((identity (nrows dictMatrix)) <|> matN )
    --
    where
        aux = dictMatrix
        _B = take (nrows dictMatrix) idxs
        sortCols currMatrix i auxMatrix = case elemIndex i idxs of
                                    Just idx -> sortCols currMatrix (i+1) (auxMatrix & colAt i .~ (currMatrix ^. colAt idx))
                                    Nothing -> auxMatrix
        sortRows currMatrix i auxMatrix = case (if i < nrows currMatrix then elemIndex (_B !! i) (sort _B) else Nothing)  of
                                    Just idx -> sortRows currMatrix (i+1) (auxMatrix & rowAt idx .~ (currMatrix ^. rowAt i))
                                    Nothing -> auxMatrix
        sortedDict = (sortRows (sortCols dictMatrix 0 aux) 0 aux )
        matN = submatrix' (0, (nrows dictMatrix)-1) (nrows dictMatrix, (ncols dictMatrix)-1) $ sortedDict


lrs :: Matrix Rational -> Col -> [Vertex]
lrs matrix b = revSearch dictionary
    where
        dictionary = sortDictionary $ simplex $ getDictionary matrix b


getVertex :: Dictionary -> Vertex
getVertex dictionary = concat $ toLists $ submatrix' (1,dim) (cols-1, cols-1) (dictionary^.dict)
    where
        rows = numRows dictionary
        cols = numCols dictionary
        dim = cols-rows-1

revSearch :: Dictionary -> [Vertex]
revSearch dictionary
    | isLexMin = getVertex dictionary : concatMap revSearch pivoted
    | otherwise = concatMap revSearch pivoted
    where
        rows = numRows dictionary
        cols = numCols dictionary
        isLexMin = lexmin dictionary 0
        valid_N = [i | i <- [rows..cols-2] , (reverseRS dictionary i) /= Nothing]
        valid_B = map (lexMinRatio dictionary) valid_N
        pivoted = safeZipWith (\r s -> pivot r s dictionary) valid_B valid_N
