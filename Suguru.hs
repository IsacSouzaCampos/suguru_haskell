module Suguru (nextPosition) where
import TableInfos

nextPosition :: [Int] -> Int -> Int
nextPosition [] _ = -1
nextPosition (a:b) n = do
  if a == -1 then
    n
  else
    nextPosition b (n + 1)


getBoxNumber :: [Int] -> Int -> Int -> Int
getBoxNumber [] _ _ = -1
getBoxNumber (a:b) n i  | i == n = a
                        | otherwise = getBoxNumber b n (i + 1)


getBoxSize :: [Int] -> Int -> Int
getBoxSize [] _ = 0
getBoxSize (a:b) box_number | a == box_number = 1 + getBoxSize b box_number
                            | otherwise = getBoxSize b box_number


--checkValidation :: [Int] -> Int -> Bool
--checkValidation [] _ = True
-- TODO


setPosition :: Int -> Bool
setPosition n = do
  let i = floor(fromIntegral n / fromIntegral _size)
  let j = n `mod` _size
  let box_number = getBoxNumber final_table_design n 0
  let box_size = getBoxSize table_design box_number
  --[checkValidation final_table x | x<-[1..box_size]]
  True
