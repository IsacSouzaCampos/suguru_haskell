module TableInfos (table_design, size, _size, final_table, final_table_design, boxes_size, boxes_positions, printTable, chop, getValidPositions) where

{-table_design é a tabela onde o formato das caixas do suguru é descrito. Cada caixa tem um número e esse número e alocado nas posições que representam a caixa.-}

type Matrix = [[Int]]

table_design = [0,   0,  0,  1,  2,  2,  3,  3,
                0,   4,  4,  1,  2,  2,  3,  3,
                4,   4,  1,  1,  2,  5,  5,  3,
                4,   6,  6,  1,  7,  7,  5,  5,
                8,   8,  6,  6,  7,  9,  9,  5,
                10,  8,  8,  6,  7, 11,  9,  9,
                10, 10,  8, 12,  7, 11, 11,  9,
                10, 10, 12, 12, 12, 12, 11, 11]

{-- Outro exemplo
table_design = [0, 0, 0, 1, 1,
                0, 2, 2, 3, 1,
                2, 2, 3, 3, 1,
                2, 3, 3, 4, 1,
                4, 4, 4, 4, 5]
--}

-- x representa as posições indefinidas da tabela
x = -1
table = [x, x, x, 3, x, x, 2, x,
         4, x, x, x, x, x, x, x,
         x, 2, x, x, x, x, x, x,
         x, 1, 5, x, x, 1, 5, x,
         x, 2, x, x, x, x, x, x,
         x, x, x, x, 4, x, x, 4,
         x, x, x, x, x, 3, x, x,
         x, 5, x, x, x, 5, x, x]

{-- Outro exemplo
table = [1, x, x, 5, x,
         x, x, x, x, x,
         1, x, 2, x, 4,
         x, x, x, x, x,
         x, 3, x, x, x]
--}

sizeFloat = sqrt (fromIntegral (length table))

-- valor inteiro que representa o número de linhas e colunas da tabela
size = round sizeFloat


{-As tabelas serão atualizadas com a criação de uma borda representada pelo número -2. Isso é feito para facilitar os testes para saber se os valores vizinhos são iguais ao valor que o software está tentando alocar no momento. Sem essa borda, quando uma posição testada fosse nas laterais ou quinas da tabela, isso exigiria cálculos específicos para teste dos vizinhos e geraria mais linhas de código a serem executados.-}

addBorder :: [Int] -> Int -> [Int]
addBorder [] _ = []
addBorder (a:b) i = do
  if i `mod` size == 0 && i > 0 then
    [(-2), (-2)] ++ [a] ++ addBorder b (i + 1)
  else if i `mod` size == 0 then
    [(-2)] ++ [a] ++ addBorder b (i + 1)
  else
    a:addBorder b (i + 1)


chop :: Int -> [Int] -> Matrix
chop _ [] = []
chop n xs = take n xs:chop n (drop n xs)


getNumOfBoxes :: [Int] -> Int -> Int
getNumOfBoxes [] n = n
getNumOfBoxes (a:b) n = getNumOfBoxes b (max a n)


count :: [Int] -> Int -> Int
count [] _ = 0
count (a:b) n | a == n = 1 + count b n
              | otherwise = count b n


getPositions :: [Int] -> Int -> Int -> [Int]
getPositions [] _ _ = []
getPositions (a:b) n i  | a == n = ((i):getPositions b n (i + 1))
                        | otherwise = getPositions b n (i + 1)


getValidPositions :: [Int] -> [Int]
getValidPositions [] = []
getValidPositions (a:b) | a < 0 = getValidPositions b
                        | otherwise = [a] ++ getValidPositions b


printTable :: Matrix -> IO()
printTable = putStrLn . unlines . map (unwords . map show)

-- um novo tamanho é calculado considerando as bordas
_size = size + 2

top_bottom_border = [(-2) | x<-[1.._size]]
temp_table = addBorder table 0
temp__table_design = addBorder table_design 0

final_table = top_bottom_border ++ temp_table ++ top_bottom_border ++ [(-2)]

final_table_design = top_bottom_border ++ temp__table_design ++ top_bottom_border ++ [(-2)]

num_of_boxes = (getNumOfBoxes table_design (-1)) + 1
boxes_size = [(x, count table_design x) | x<-[0..num_of_boxes - 1]]
boxes_positions = [(x, getPositions final_table_design x 0) | x<-[0..num_of_boxes - 1]]
