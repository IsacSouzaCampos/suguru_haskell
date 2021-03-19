import TableInfos
import Suguru

main = do
  let next = nextPosition final_table [] 0 0
  --putStrLn (show (getFirst next))
  --putStrLn (show (getSecond next))
  let resulting_table = getSecond (setPosition (getSecond next) (getFirst next))
  let correctedTable = correctFinalTable (resulting_table)
  --putStrLn (show (getValidPositions resulting_table))
  printTable (chop 8 (correctedTable))


  {--
  let n = 18
  let updt_table = [-2,-2,-2,-2,-2,-2,-2,-2,-2,-2,-2,1,3,2,3,1,3,2]
  putStrLn ("n = " ++ show n)
  putStrLn ("updt_table = " ++ show updt_table)

  -- setPosition
  let box_number = getBoxNumber final_table_design n 0
  let box_size = getBoxSize table_design box_number
  let v = 1
  putStrLn ("\nbox_number = " ++ show box_number)
  putStrLn ("box_size = " ++ show box_size)
  
  -- checkValidation
  let neighborhoodClean = isNeighborhoodClean updt_table n v
  let boxClean = isBoxClean updt_table (getBoxPositions box_number boxes_positions) v
  
  if neighborhoodClean && boxClean then do
    let next1 = nextPosition final_table (updt_table ++ [v]) 0 (n + 1)
    let n1 = getFirst next1
    let updt_table1 = getSecond next1
    putStrLn ("\nn1 = " ++ show n1)
    putStrLn ("updt_table1 = " ++ show updt_table1)
  
  else do
    let v1 = v + 1
    putStrLn ("\nv = " ++ show v1)
    --}
  