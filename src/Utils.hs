module Utils (within, toNumber) where

import Data.Maybe

within :: (Int, Int) -> Int -> Bool
within (a, b) c = let (smaller, greater) = if a > b then (b, a) else (a, b) in
                  c >= smaller && c <= greater

toNumber :: [Int] -> Int
toNumber xs = toNumber' (reverse xs)
  where 
    toNumber' [x]     = x
    toNumber' (x: xs) = x + 10 * toNumber' xs 

bfs :: a -> (a -> [a]) -> (a -> Bool) -> Maybe a     
bfs root getChildren utilityFunction = undefined -- TODO implement using queue