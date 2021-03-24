module Suguru (setPosition, nextPosition, getFirst, getSecond) where
import TableInfos

{-getBoxNumber: Recebe a table_design e uma posição como entradas e retorna o id da caixa em que a posição se encontra-}
getBoxNumber :: [Int] -> Int -> Int -> Int
getBoxNumber [] _ _ = -1
getBoxNumber (a:b) n i  | i == n = a
                        | otherwise = getBoxNumber b n (i + 1)

--
{-getBoxSize: Retorna o tamanho da caixa com id dado como entrada-}
getBoxSize :: [Int] -> Int -> Int
getBoxSize [] _ = 0
getBoxSize (a:b) box_number | a == box_number = 1 + getBoxSize b box_number
                            | otherwise = getBoxSize b box_number

--
{-getBoxPositions: Retorna lista com as posições da caixa com número de id dado na entrada do método-}
getBoxPositions :: Int -> [(Int, [Int])] -> [Int]
getBoxPositions _ [] = []
getBoxPositions n (a:b) | getFirst a == n = getSecond a
                        | otherwise = getBoxPositions n b

--
{-getFirst: Retorna primeiro item da tupla dada como entrada-}
getFirst :: (t, [t]) -> t
getFirst (frst, _) = frst

{-getSecond: Retorna segundo item da tupla dada como entrada-}
getSecond :: (t, [Int]) -> [Int]
getSecond (_, []) = []
getSecond (_, scnd) = scnd

{-isAllTrue: Analisa se todos os valores dentro da lista de booleanos dada como entrada é True. Se sim, retorna True. Se não, False-}
isAllTrue :: [Bool] -> Bool
isAllTrue [] = True
isAllTrue (a:b) | a == True = isAllTrue b
                | otherwise = False

--
{-isPositionValid: Função chamada por 'isNeighborhoodClean' e 'isBoxClean' que verifica se a posição atual da análise desses métodos está de acordo com os requisitos do suguru-}
isPositionValid :: [Int] -> Int -> Int -> Int -> Bool
isPositionValid [] _ _ _ = True
isPositionValid (a:b) i n v = do
  if i == n then
    a /= v
  else
    isPositionValid b (i + 1) n v

--
{-isBoxClean: Verifica se a caixa da posições em análise tem valores de acordo com o valor testado-}
isBoxClean :: [Int] -> [Int] -> Int -> Bool
isBoxClean [] _ _ = True
isBoxClean updt_table box_positions v = do
  let boolList_ft = [isPositionValid final_table 0 n v | n<-box_positions]
  let boolList_ut = [if n < (length updt_table) then (isPositionValid updt_table 0 n v) else True | n<-box_positions]

  isAllTrue (boolList_ft ++ boolList_ut)

--
{-isNeighborhoodClean: Verifica se as posições vizinhas à posição em análise estão de acordo com o valor testado-}
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
{-nextPosition: Retorna a próxima posição indefinida que deverá ser ajustada (e -1 caso a tabela já tenha sido completamente percorrida). Durante a execução da função, as posições previamente definidas da tabela são adicionadas à lista updt_table (que representa a tabela final atualizada)-}
nextPosition :: [Int] -> [Int] -> Int ->  Int -> (Int, [Int])
--              ft    -> ut    -> i   ->  n
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
{-checkValidation: Verifica se o valor a ser adicionado à posição requerida está dentro dos requisitos necessários. Esta função é a responsável por manter o loop responsável pela varredura da tabela analisada-}
checkValidation :: [Int] -> [Int] -> Int -> Int -> Int -> Int -> (Bool, [Int])
checkValidation [] _ _ _ _ _ = (False, [])
checkValidation f_table updt_table n box_n v vf = do
  let neighborhoodClean = isNeighborhoodClean updt_table n v
  let boxClean = isBoxClean updt_table (getBoxPositions box_n boxes_positions) v
  
  -- confirma se o valor de entrada está dentro do limite de tamanho da caixa
  if v <= vf then
    if neighborhoodClean && boxClean then do
      let next = nextPosition final_table (updt_table ++ [v]) 0 (n + 1)
      let new_n = getFirst next
      
      --verifica se a tabela foi completamente percorrida (n = -1)
      if new_n < 0 then
        --se a tabela foi completamente varrida, retorna True com a tabela resultante atualizada com a última posição
        (True, updt_table ++ [v])
      
      --dá continuidade à recursão
      else do
        let (isFinished, new_updt_table) = setPosition (getSecond next) new_n
        if not isFinished then
          checkValidation f_table updt_table n box_n (v + 1) vf
        else
          (True, new_updt_table)

    --se o valor atual não cumpre com os requisitos, testa o próximo valor (v + 1)
    else
      checkValidation f_table updt_table n box_n (v + 1) vf
  
  --se o valor atual ultrapassa o limite de tamanho da caixa, retorna False para que a posição anterior testada seja modificada
  else
    (False, [])

--
{-setPosition: Recebe a tabela a ser atualizada com as novas posições e a posição atual a ser calculada. Captura as estruturas de dados necessárias para os cálculos de validação da posição-}
setPosition :: [Int] -> Int -> (Bool, [Int])
setPosition updt_table n = do
  let box_number = getBoxNumber final_table_design n 0
  let box_size = getBoxSize table_design box_number
  
  checkValidation final_table updt_table n box_number 1 box_size
