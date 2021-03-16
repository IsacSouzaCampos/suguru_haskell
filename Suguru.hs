module Suguru (isPositionValid, isNeighborhoodClean, nextPosition, getBoxPositions, getFirst, getSecond, setPosition) where
import TableInfos


getBoxNumber :: [Int] -> Int -> Int -> Int
getBoxNumber [] _ _ = -1
getBoxNumber (a:b) n i  | i == n = a
                        | otherwise = getBoxNumber b n (i + 1)

--
getBoxSize :: [Int] -> Int -> Int
getBoxSize [] _ = 0
getBoxSize (a:b) box_number | a == box_number = 1 + getBoxSize b box_number
                            | otherwise = getBoxSize b box_number

--
getBoxPositions :: Int -> [(Int, [Int])] -> [Int]
getBoxPositions _ [] = []
getBoxPositions n (a:b) | getFirst a == n = getSecond a
                        | otherwise = getBoxPositions n b

--
getFirst :: (t, [t]) -> t
getFirst (frst, _) = frst

--
getSecond :: (t, [Int]) -> [Int]
getSecond (_, []) = []
getSecond (_, scnd) = scnd

--
{--numberOfStepsBack :: [Int] -> Int -> Int -> Int -> Int
numberOfStepsBack [] _ = 0
numberOfStepsBack (a:b) n i last_postition = do
  if i == n then
    n - last_postition
  else if a == -1 then
    numberOfStepsBack b n (i + 1) (i)
  else
    numberOfStepsBack b n (i + 1) last_postition--}

--
{--updateTable :: [Int] -> Int -> Int -> (Int, [Int])
updateTable [] _ = (-1, [])
updateTable (a:b) last_position_to_copy i | i == last_position_to_copy = [a]
                                          | otherwise = updateTable b last_position_to_copy (i + 1) --}

-- chamado do isNeighborhoodClean e isBoxClean
isAllTrue :: [Bool] -> Bool
isAllTrue [] = True
isAllTrue (a:b) | a == True = isAllTrue b
                | otherwise = False

-- chamado do isNeighborhoodClean e isBoxClean
isPositionValid :: [Int] -> Int -> Int -> Int -> Bool
isPositionValid [] _ _ _ = False
isPositionValid (a:b) i n v = do
  if i == n then
    a /= v
  else
    isPositionValid b (i + 1) n v

--
isBoxClean :: [Int] -> Int -> Bool
isBoxClean [] _ = True
isBoxClean box_positions v = do
  let boolList = [isPositionValid final_table 0 n v | n<-box_positions]

  isAllTrue boolList

--
isNeighborhoodClean :: [Int] -> Int -> Int -> Bool
isNeighborhoodClean updt_table n v = do
  let p1 = (n - _size - 1)
  let p2 = (n - _size)
  let p3 = (n - _size + 1)
  let p4 = (n - 1)
  
  let ut_positions = [p1, p2, p3, p4]
  let ft_positions = ut_positions ++ [(n + 1), (n + _size - 1), (n + _size), (n + _size + 1)]

  
  let boolList1 = [isPositionValid final_table 0 n v | n<-ft_positions]
  let boolList2 = [isPositionValid updt_table 0 n v | n<-ut_positions]

  isAllTrue (boolList1 ++ boolList2)

--
checkValidation :: [Int] -> [Int] -> Int -> Int -> Int -> Int -> (Bool, [Int])
checkValidation [] _ _ _ _ _ = (False, [])
checkValidation f_table updt_table n box_n v vf = do
  let neighborhoodClean = isNeighborhoodClean updt_table n v
  let boxClean = isBoxClean (getBoxPositions box_n boxes_positions) v
  
  if v <= vf then
    if neighborhoodClean && boxClean then do
      let next = nextPosition final_table (updt_table ++ [v]) 0 (n + 1)
      let (isOk, new_updt_table) = setPosition (getSecond next) (getFirst next)
      
      if not isOk then
        checkValidation f_table updt_table n box_n (v + 1) vf
      else
        (True, new_updt_table)

    else
      checkValidation f_table updt_table n box_n (v + 1) vf
  
  else
    --let last_position_to_copy = numberOfStepsBack final_table n 0 0
    --let (n, new_updt_table) = updateTable updt_table last_position_to_copy 0
    (False, [])
    --False
  --neighborhoodClean && boxClean

--              ft    -> ut    -> i   ->  n
nextPosition :: [Int] -> [Int] -> Int ->  Int -> (Int, [Int])
nextPosition [] _ _ _ = (-1, [])
nextPosition (a:b) xs i n = do
  if i >= n then
    if a == -1 then
      (i, xs)
    else
      nextPosition b (xs ++ [a]) (i + 1) n
  else
    nextPosition b xs (i + 1) n

--
setPosition :: [Int] -> Int -> (Bool, [Int])
setPosition updt_table n = do
  --let i = floor(fromIntegral n / fromIntegral _size)
  --let j = n `mod` _size
  let box_number = getBoxNumber final_table_design n 0
  let box_size = getBoxSize table_design box_number
  
  checkValidation final_table updt_table n box_number 1 box_sizez
  
