import TableInfos
import Suguru

main = do
  --printTable matrix_final_table
  --print(isNeighborhoodClean final_table final_table_design 22 4)
  --print(nextPosition final_table [] 0 0)
  --print(getBoxPositions 4 boxes_positions)
  let next = nextPosition final_table [] 0 0
  let n = getFirst next
  let updt_table = getSecond next
  print(n)
  print(updt_table)
  print(setPosition updt_table n)
