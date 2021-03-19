module Suguru (setPosition, nextPosition, getFirst, getSecond) where
import TableInfos
import Debug.Trace


getBoxNumber :: [Int] -> Int -> Int -> Int
getBoxNumber [] _ _ = -1
getBoxNumber (a:b) n i  | i == n = a
                        | otherwise = getBoxNumber b n (i + 1)

--
--
getBoxSize :: [Int] -> Int -> Int
getBoxSize [] _ = 0
getBoxSize (a:b) box_number | a == box_number = 1 + getBoxSize b box_number
                            | otherwise = getBoxSize b box_number

--
--
getBoxPositions :: Int -> [(Int, [Int])] -> [Int]
getBoxPositions _ [] = []
getBoxPositions n (a:b) | getFirst a == n = getSecond a
                        | otherwise = getBoxPositions n b

--
--
getFirst :: (t, [t]) -> t
getFirst (frst, _) = frst

--
getSecond :: (t, [Int]) -> [Int]
getSecond (_, []) = []
getSecond (_, scnd) = scnd

-- chamado do isNeighborhoodClean e isBoxClean
isAllTrue :: [Bool] -> Bool
isAllTrue [] = True
isAllTrue (a:b) | a == True = isAllTrue b
                | otherwise = False

--
-- chamado do isNeighborhoodClean e isBoxClean
isPositionValid :: [Int] -> Int -> Int -> Int -> Bool
isPositionValid [] _ _ _ = True
isPositionValid (a:b) i n v = do
  if i == n then
    a /= v
  else
    isPositionValid b (i + 1) n v

--
--
isBoxClean :: [Int] -> [Int] -> Int -> Bool
isBoxClean [] _ _ = True
isBoxClean updt_table box_positions v = do
  let boolList_ft = [isPositionValid final_table 0 n v | n<-box_positions]
  let boolList_ut = [if n < (length updt_table) then (isPositionValid updt_table 0 n v) else True | n<-box_positions]

  isAllTrue (boolList_ft ++ boolList_ut)

--
--
isNeighborhoodClean :: [Int] -> Int -> Int -> Bool
isNeighborhoodClean updt_table n v = do
  let p1 = (n - _size - 1)
  let p2 = (n - _size)
  let p3 = (n - _size + 1)
  let p4 = (n - 1)
  
  let ut_positions = [p1, p2, p3, p4]
  let ft_positions = ut_positions ++ [(n + 1), (n + _size - 1), (n + _size), (n + _size + 1)]

  
  let boolList_ft = [isPositionValid final_table 0 n v | n<-ft_positions]
  let boolList_ut = [isPositionValid updt_table 0 n v | n<-ut_positions]

  isAllTrue (boolList_ft ++ boolList_ut)

--
--              ft    -> ut    -> i   ->  n
nextPosition :: [Int] -> [Int] -> Int ->  Int -> (Int, [Int])
nextPosition [] _ _ _ = (-1, [])
nextPosition (a:b) updt_table i n = do

  let table_length = (_size * _size)
  if n > table_length || i > table_length  then
    (-1, updt_table)
  else if i >= n then
    if a == -1 then
      (i, updt_table)
    else
      nextPosition b (updt_table ++ [a]) (i + 1) n
  else
    nextPosition b updt_table (i + 1) n

--
--
checkValidation :: [Int] -> [Int] -> Int -> Int -> Int -> Int -> (Bool, [Int])
checkValidation [] _ _ _ _ _ = (False, [])
checkValidation f_table updt_table n box_n v vf = do
  let neighborhoodClean = isNeighborhoodClean updt_table n v
  let boxClean = isBoxClean updt_table (getBoxPositions box_n boxes_positions) v
  
  if v <= vf then
    if neighborhoodClean && boxClean then do
      let next = nextPosition final_table (updt_table ++ [v]) 0 (n + 1)
      let new_n = getFirst next
      
      if new_n < 0 then
        (True, updt_table ++ [v])
      else do
        let (isFinished, new_updt_table) = setPosition (getSecond next) new_n
        if not isFinished then
          checkValidation f_table updt_table n box_n (v + 1) vf
        else
          (True, new_updt_table ++ [v])

    else
      checkValidation f_table updt_table n box_n (v + 1) vf
  
  else
    (False, [])

--
--
setPosition :: [Int] -> Int -> (Bool, [Int])
setPosition updt_table n = do
  let box_number = getBoxNumber final_table_design n 0
  let box_size = getBoxSize table_design box_number
  
  checkValidation final_table updt_table n box_number 1 box_size
