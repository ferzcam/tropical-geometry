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
import Numeric.LinearProgramming hiding (simplex)
import Data.List

type Row = Matrix Rational
type Col = Matrix Rational

data Dictionary = Dict {
                            __B :: [Int], 
                            __N :: [Int], 
                            _dict :: Matrix Rational
                        } 
    deriving (Show, Eq)

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




getOptimumVertex :: Matrix Rational -> Col -> Maybe Vertex
getOptimumVertex mat col = trace ("Problem: " ++ show (replicate (ncols mat) (-1))  ++ "\n\n Result :" ++ show result ) fmap (map toRational) $ feasNOpt result
    where
        problem = Maximize $ replicate (ncols mat) (-1)
        constraints = Dense $ safeZipWith (:<=:) (map (map fromRational) $ toLists mat) (map fromRational $ concat $ toLists col)
        result = exact problem constraints (map Free [1..ncols mat])
        feasNOpt (Optimal (_, vertex)) = Just vertex
        feasNOpt _ = Nothing



{- 
    Dictionary form

                p21     p31
    invA_b
                p22     p32

 -}






--  getDictionary :: Matrix Rational -> Col -> Dictionary
--  getDictionary _A b = Dict [0..rows+1+cols] (identity (rows+1)  <|> (p21 <-> p22)  <|> (p31 <-> p32))
--      where
--          rows = nrows _A
--          cols = ncols _A
--          slack = identity rows
--  --        topRow = rowFromList $ 1: (replicate (cols) (-1)) ++ (replicate rows 0)
--          topRow = rowFromList $ 1 : (replicate (rows) 0) ++ (replicate (cols) (-1))
--  
--          leftColumn = colFromList (replicate rows 0)
--          newA =  _A <|> slack --matrix form
--          _A_b = submatrix' (0, rows-1) (0, rows-1) newA -- basis
--          _A_n = submatrix' (0, rows-1) (rows, rows+cols-1) newA -- cobasis
--          c_b = submatrix' (0, 0) (1, rows) topRow -- basic part of top row
--          c_n = submatrix' (0, 0) (rows+1, rows+cols) topRow -- cobasic part of top row
--          -- parts of the dictionary 
--          Right invA_b = inverse _A_b
--          p21 = negate $ c_n - (c_b |*| invA_b |*| _A_n)
--          p22 = invA_b |*| _A_n
--          p31 =  c_b |*| invA_b |*| b
--          p32 = invA_b |*| b

getDictionary :: Matrix Rational -> Col -> Dictionary
getDictionary _A b = trace ("invA_b>>> " ++ show (invA_b) ++ "\n\nAB" ++ show (_A_b)) Dict [0..rows] [rows+1..rows+cols] (identity (rows+1)  <|> (p21 <-> p22)  <|> (p31 <-> p32))
    where
        rows = nrows _A
        cols = ncols _A
        slack = identity rows
--        topRow = rowFromList $ 1: (replicate (cols) (-1)) ++ (replicate rows 0)
        topRow = rowFromList $ 1: ((replicate (cols) (-1) ++ replicate (rows) 0))

        leftColumn = colFromList (replicate rows 0)
        newA =  topRow <-> (leftColumn <|>  _A <|> slack) --matrix form
        newB = (colFromList [0]) <-> b
        _A_b = submatrix' (1, rows) (1, rows) newA -- basis
        _A_n = submatrix' (1, rows) (rows+1, rows+cols) newA -- cobasis
        -- parts of the dictionary 
        c_b = submatrix' (0, 0) (1, rows) topRow -- basic part of top row
        c_n = submatrix' (0, 0) (rows+1, rows+cols) topRow -- cobasic part of top row
        Right invA_b = inverse _A_b
        p22 = invA_b |*| _A_n
        p32 = invA_b |*| b
        p21 = negate $ c_n - (c_b |*| invA_b |*| _A_n)
        p31 = c_b |*| invA_b |*| b
--          p21 = negate $ c_n - (c_b |*| invA_b |*| _A_n)
--          p22 = invA_b |*| _A_n
--          p31 =  c_b |*| invA_b |*| b
--          p32 = invA_b |*| b


        

getSortedDictionary :: Matrix Rational -> Col -> Dictionary
getSortedDictionary mat col = trace ("PAIRS>>> " ++ show toBeLast ) getDictionary newMat newCol
    where
        optimum = fromJust $ getOptimumVertex mat col
        matLists = toLists mat
        bList = concat $ toLists col
        meetEq = [i | i <- [0..((pred.nrows) mat)], (dot (matLists!!i) optimum) == col^. elemAt (i,0)]
        pairs = safeZipWith (,) matLists bList
        toBeLast = map (pairs !!) meetEq
        ordered = (pairs \\ toBeLast) ++ (toBeLast)
        newMat = fromLists $ map fst ordered
        newCol = colFromList $ map snd ordered

getSortedDictionary' :: Matrix Rational -> Col -> (Matrix Rational, Col)
getSortedDictionary' mat col = trace ("PAIRS>>> " ++ show toBeLast) (,) newMat newCol
    where
        optimum = fromJust $ getOptimumVertex mat col
        matLists = toLists mat
        bList = concat $ toLists col
        meetEq = [i | i <- [0..((pred.nrows) mat)], (dot (matLists!!i) optimum) == col^. elemAt (i,0)]
        pairs = safeZipWith (,) matLists bList
        toBeLast = map (pairs !!) meetEq
        ordered = (pairs \\ toBeLast) ++ (reverse $ sort toBeLast)
        newMat = fromLists $ map fst ordered
        newCol = colFromList $ map snd ordered


simplex :: Dictionary -> Dictionary
simplex dictionary
    | idxsToPivot == Nothing = dictionary
    | otherwise = simplex $ uncurry pivot (fromJust idxsToPivot) dictionary
    where
        idxsToPivot = selectPivot dictionary

-- enteringVariable :: (Num a, Ord a, Show a) => 
--     [Int] ->    -- | Cobasis N
--     [a] ->      -- | Row zero of A_N
--     Maybe Int   -- | Index of the entering variable in N
-- enteringVariable _N cobasic  = findIndex (<0) cobasic
    -- | null idxsNeg = Nothing
    -- | otherwise = elemIndex leastCob _N
    -- where
    --     idxsNeg = [i | i <- [0..(length cobasic)-1],  cobasic!!i < 0]
    --     leastCob = minimum $ map (_N!!) idxsNeg
    --fmap ((+) (dimB)) $ findIndex (<0) cobasic


enteringVariable ::
    Dictionary ->      -- | Row zero of A_N
    Maybe Int   -- | Index of the entering variable in N
enteringVariable dictionary  = findIndex (<0) negs
    where
        negs = map (\j -> dictionary^.dict.elemAt (0,j)) (dictionary^._N)



selectPivot :: Dictionary -> Maybe (Int, Int)
selectPivot dictionary
    | idxEntering == Nothing  =    Nothing
    | idxLeaving == Nothing  =   Nothing
    | otherwise = trace ("\n Pivot " ++ show (((fromJust idxLeaving), (cobasis!!(fromJust idxEntering)) ) ) ++ "\n Cobase>>>"  ++ show cobasis  ++ "\nDictionary" ++ show dictMatrix )   Just $ (,) (fromJust idxLeaving) (cobasis!!(fromJust idxEntering))
    where
        cobasis = dictionary^._N
        dictMatrix = dictionary^.dict
        rows = nrows dictMatrix
        cols = ncols dictMatrix
        dim = cols - rows - 1
        idxEntering = (enteringVariable dictionary)
        idxLeaving = lexMinRatio dictionary (fromJust idxEntering)
        -- (cobasis!!(fromJust idxEntering))

lexMinRatio :: 
    Dictionary ->   -- | Dictionary
    Int ->          -- | Index of entering varibale s in N
    Maybe Int             -- | Variable s in B
lexMinRatio dictionary s
    | null indexed_s = Nothing
    | otherwise = trace ("\n D-Vectors>>>" ++ show sub_D) Just $ (dictionary^._B) !! (fst $ indexed_s !! (fromJust $ elemIndex (minimum ratios) ratios))
    where
       -- sizeB = length $ dictionary^._B
        dictMatrix = dictionary^.dict
        rows = nrows dictMatrix
        cols = ncols dictMatrix
        dim = cols - rows - 1
        _D = (dictMatrix ^. colAt (cols-1)) <|> submatrix' (0, rows-1) (0, rows-1) dictMatrix
        s_col = dictMatrix ^. colAt ((dictionary^._N)!!s) -- (sizeB + s)
        slice_s = (concat . toLists . (submatrix' (dim+1, rows-1) (0,0)))  s_col
        indexed_s = filter (snd.(fmap (>0))) $ zip [(dim+1)..] slice_s
        sub_D = map (head .toLists . (\i -> _D ^. rowAt i) . fst) indexed_s -- takes rows with indices as in indexed_s
        ratios = safeZipWith (map $) (map ((flip (/)) . snd) indexed_s) sub_D


-- lexMinRatio :: 
--     Dictionary ->   -- | Dictionary
--     Int ->          -- | Index of entering variable s in N
--     Int             -- | Index of possible leaving variable s in B
-- lexMinRatio dictionary s
--     | null indexed_s = 0
--     | otherwise = (dictionary ^. _B )!! (fst $ indexed_s !! (fromJust $ elemIndex (minimum ratios) ratios))
-- --  | otherwise = (dictionary ^. _B )!! (fst $ indexed_s !! (fromJust $ elemIndex (minimum ratios) ratios))
--     -- 
--     where
--         sizeB = length $ dictionary^._B
--         dictMatrix = dictionary^.dict
--         rows = nrows dictMatrix
--         cols = ncols dictMatrix
--         dim = cols - rows - 1
--         _D = (dictMatrix ^. colAt (cols-1)) <|> submatrix' (0, rows-1) (0, rows-1) dictMatrix
--         s_col = dictMatrix ^. colAt pivot
--         slice_s = (concat . toLists . (submatrix' (dim+1, rows-1) (0,0)))  s_col
--         indexed_s = filter (snd.(fmap (>0))) $ zip [(dim+1)..] slice_s
--         sub_D = map (head .toLists . (\i -> _D ^. rowAt i) . fst) indexed_s -- takes rows with indices as in indexed_s
--         ratios = safeZipWith (map $) (map ((flip (/)) . snd) indexed_s) sub_D


-- selectPivot :: Dictionary -> Maybe (Int, Int)
-- selectPivot dictionary
--     | idxEntering == Nothing  =  trace ( "\ncobasis>>>" ++ show cobasic ++ "\nN>>>  "  ++ show (dictionary^._N) ++ "\nDic>>>\n" ++ show dictMatrix) Nothing
--     | otherwise =   Just $ (,) idxLeaving (fromJust idxEntering)
--     where
--         dictMatrix = dictionary^.dict
--         rows = nrows dictMatrix
--         cols = ncols dictMatrix
--         dim = cols - rows - 1
--         cobasic = (head.toLists.(submatrix' (0, 0) (rows, rows+dim-1))) dictMatrix 
--         idxEntering = fmap ((dictionary^._N) !!) (enteringVariable (dictionary^._N) cobasic)
--         idxLeaving = lexMinRatio dictionary (fromJust idxEntering)
            --lexMinRatio dictionary (fromJust idxEntering)
-- trace ("Pivot>>>" ++ show (Just $ (,) idxLeaving (fromJust idxEntering)) ++ "\n Basis >>" ++ show (dictionary^._B) ++ "\t Cobasis >>" ++ show (dictionary^._N) ++ "\nDic>> \n" ++ show dictMatrix)

-- Int -> -- | Basic index 0...m-1 
-- Int -> -- | Cobasic index 0..d-1
pivot ::    Int -> -- | Basic element
            Int -> -- | Cobasic element
            Dictionary -> 
            Dictionary
pivot r s dictionary
    | a_t /= 0 = trace ("\nPivot>>" ++ show (r,s) ++ "\tVertex>>>" ++ show vertex ++ "\n")   newDic
    | otherwise = dictionary
    where
        idxs_dic = dictionary^._B ++ dictionary^._N
        id_cob = fromJust $ elemIndex s (dictionary^._N)
        id_bas = fromJust $ elemIndex r (dictionary^._B)
        idx_r = fromJust $ elemIndex r idxs_dic
        idx_s = fromJust $ elemIndex s idxs_dic
        dictMatrix = dictionary^.dict
        col_s = dictMatrix ^. colAt (s)
        a_t = col_s ^. elemAt (idx_r, 0)
        eta = (mapCol' (\_ x -> -x/a_t) 0 col_s) & elemAt (idx_r,0) .~ (1/a_t)
        _E = (identity (nrows dictMatrix)) & colAt (idx_r) .~ eta
        newDic = Dict (dictionary^._B & element id_bas .~ s ) (dictionary^._N & element id_cob .~ r) (_E |*| dictMatrix)
        vertex = getVertex newDic
-- trace ("\n\nNvec>>>" ++ show (dictionary^._N) ++ "\tReplaced>>>" ++ show (dictionary^._N & element id_cob .~ _B_r) ++ "\n s>>> " ++  show id_cob ++ "\t r>>>" ++ show id_bas ++ "\n")
-- trace ("\nDic>>>> \n" ++ show dictMatrix  ++ "\nCol_S>>>> \n" ++ show col_s ++ "\n a>>> " ++ show a_t )




-- lexmin :: Dictionary -> Int -> Bool
-- lexmin dictionary _
--     | s == Nothing = True
--     | otherwise = not $ _B_r>_N_s && (dictMatrix ^. elemAt (r,rows-1)) == 0 && (s_col ^. elemAt (r,0)) /= 0
--     where
--         sizeB = length $ dictionary^._B
--         dictMatrix = dictionary^.dict
--         rows = nrows dictMatrix
--         cols = ncols dictMatrix
--         dim = cols - rows - 1
--         cobasic = (head.toLists.(submatrix' (0, 0) (rows, rows+dim-1))) dictMatrix 
--         s = enteringVariable (dictionary^._N) cobasic
--         r = fromJust $ lexMinRatio dictionary (fromJust s)
--         _B_r = (dictionary ^._B) !! (r)
--         _N_s = (dictionary ^._N) !! (fromJust s)
--         s_col = dictMatrix ^. colAt (sizeB + (fromJust s))



reverseRS' :: 
    Dictionary ->
    Int -> -- | Index of the element i the N
    Maybe ([Int], Int)
reverseRS' dictionary v
    | v<0 || v>= length (dictionary^._N) = error "reverseRS: index of N out of bound"
    | filtered == [] = trace ("\nPreFiltered>>>\n"++ show filt_aux) Nothing
    | otherwise =  trace ("\nPreFiltered>>>\n"++ show filt_aux) Just ( map (snd) filtered, v_elem)
    where
        basis = dictionary^._B
        cobasis = dictionary^._N
        v_elem = cobasis!!v
        dictMatrix = dictionary^.dict
        possibleBasis = zip (map (\x -> pivot x v_elem dictionary ) basis) basis
        getPivots = map (\dic ->  (selectPivot $  fst dic, snd dic) ) possibleBasis
        filt_aux = filter (\(piv, u) ->  piv /= Nothing ) getPivots
        filtered = filter (\(piv, u) ->   ( (fst $ fromJust piv) == v_elem) && (( snd $ fromJust piv ) == u) ) filt_aux
--trace ("\n\n Posible Basis >>>" ++ show possibleBasis ++ "\n\n Get Pivots>>>" ++ show getPivots  ++ "\nMatrix >>>" ++ show dictMatrix  ++ "\nDic B~ >>>\n" ++ show dic1 ++  "\nDic B>>>\n" ++ show dic2)
        
         



reverseRS :: Dictionary -> Int -> Maybe ( [Int], Int)
reverseRS dictionary v
    | v<0 || v>= length (dictionary^._N) = error "reverseRS: index of N out of bound"
    | conditions == False = trace ("\nWro_U>>>" ++ show w_row_u ++ "\nDiff Ws" ++ show  diff_ws ++ "\nW0>>>" ++ show w_row_0 ++ "\n List>>>"++ show lista )  Nothing
    | conditions == True = trace ("\nWro_U>>>" ++ show w_row_u ++ "\nDiff Ws" ++ show  diff_ws ++ "\nW0>>>" ++ show w_row_0 ++ "\n List>>>"++ show lista ) Just ([u], v_el )
    where
        v_el = (dictionary^._N)!!v
        rows = numRows dictionary
        cols = numCols dictionary
        dictMatrix = dictionary^.dict
        v_col = dictMatrix ^. colAt (v_el)
        w_row_0 = dictMatrix ^. rowAt 0
        u = fromJust $ lexMinRatio dictionary v
        i = fromJust $ elemIndex u (dictionary^._B)
        w_row_u = mapRow' (\_ w -> (v_col ^. elemAt (0,0))/(v_col ^. elemAt (i,0)) * w) 0 $ dictMatrix ^. rowAt (i)
        diff_ws = (head.toLists) $ w_row_0 - w_row_u
        lista = [(diff_ws!!((dictionary^._N)!!j))| j <- [0..(length (dictionary^._N))-1] , (dictionary^._N)!!j < u]
        conditions = (w_row_0 ^. elemAt (0,v_el)) > 0 && u /= 0  && all (>=0) lista



-- && u /= Nothing
--  && (dictionary^.idxs)!!(fromJust u) /= 0 

--[0..rows] [rows+1..rows+cols]

sortDictionary :: Dictionary -> Dictionary
sortDictionary (Dict _B _N dictMatrix) =  Dict [0..rows-1] [rows..rows+dim-1] ((identity (nrows dictMatrix)) <|> matN )
    --
    where
        dim = cols - rows - 1
        rows = nrows dictMatrix
        cols = ncols dictMatrix
        aux = dictMatrix
        idxs = _B ++ _N
        sortCols currMatrix i auxMatrix = case elemIndex i idxs of
                                    Just idx -> sortCols currMatrix (i+1) (auxMatrix & colAt i .~ (currMatrix ^. colAt idx))
                                    Nothing -> auxMatrix
        sortRows currMatrix i auxMatrix = case (if i < nrows currMatrix then elemIndex (_B !! i) (sort _B) else Nothing)  of
                                    Just idx -> sortRows currMatrix (i+1) (auxMatrix & rowAt idx .~ (currMatrix ^. rowAt i))
                                    Nothing -> auxMatrix
        sortedDict = (sortRows (sortCols dictMatrix 0 aux) 0 aux )
        matN = submatrix' (0, (nrows dictMatrix)-1) (nrows dictMatrix, (ncols dictMatrix)-1) $ sortedDict
-- trace ("\n Dic : \n"  ++ show aux ++ "\nDic Sorted: \n" ++ show (((identity (nrows dictMatrix)) <|> matN )))

lrs :: Matrix Rational -> Col -> [Vertex]
lrs matrix b = (sort.nub) $ revSearch dictionary
    where
        --dictionary = sortDictionary $ simplex $ getSortedDictionary matrix b
        dictionary = simplex $ getSortedDictionary matrix b
-- sortDictionary $

getVertex :: Dictionary -> Vertex
getVertex dictionary = concat $ toLists $ submatrix' (1,dim) (cols-1, cols-1) (dictionary^.dict)
    where
        rows = numRows dictionary
        cols = numCols dictionary
        dim = cols-rows-1

revSearch :: Dictionary -> [Vertex]
revSearch dictionary@(Dict _B _N dictMatrix) =  getVertex dictionary : concatMap revSearch (pivoted)
 --   | isLexMin || not isLexMin = getVertex dictionary : concatMap revSearch pivoted
 --   | otherwise = concatMap revSearch pivoted
    where
        --rows = numRows dictionary
        --cols = numCols dictionary
        -- isLexMin = lexmin dictionary 0
        valid_N =  [ fromJust $ (reverseRS dictionary i) | i <- [0..(length _N)-1] , (reverseRS dictionary i) /= Nothing ]
        pivoted =  concat $ map (\(r,s) ->   (map ( \u -> pivot u s dictionary) r )  ) valid_N
        --pivoted =  map (\(r,s) ->  pivot (r!!0) s dictionary)   valid_N
        -- valid_B = map (lexMinRatio dictionary) valid_N
        -- interm = filter (\(a,b) -> a /= Nothing) $ zip valid_B valid_N
--  trace ("\nValid_N>>>" ++ show valid_N ++ "\nPivoted>>>\n" ++ show pivoted )        
