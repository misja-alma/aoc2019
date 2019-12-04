module Utils (within, toNumber) where

within :: (Int, Int) -> Int -> Bool
within (a, b) c = let (smaller, greater) = if a > b then (b, a) else (a, b) in
                  c >= smaller && c <= greater

toNumber :: [Int] -> Int
toNumber xs = toNumber' (reverse xs)
  where 
    toNumber' [x]     = x
    toNumber' (x: xs) = x + 10 * toNumber' xs 