module Suguru (nextPosition) where

nextPosition :: [Int] -> Int -> Int
nextPosition [] _ = -1
nextPosition (a:b) n = do
  if a == -1 then
    n
  else
    nextPosition b (n + 1)

setPosition :: Int -> Bool
setPosition n = True
