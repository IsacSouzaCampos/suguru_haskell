import TableInfos
import Suguru

main = do
  let next = nextPosition final_table [] 0 0
  let resulting_table = getSecond (setPosition (getSecond next) (getFirst next))
  printTable (chop 8 (getValidPositions resulting_table))
  