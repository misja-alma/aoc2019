module Utils (within, toNumber, sliding, count) where

import Data.Maybe
import qualified Data.Dequeue as D
import qualified Data.Set as S

within :: (Int, Int) -> Int -> Bool
within (a, b) c = let (smaller, greater) = if a > b then (b, a) else (a, b) in
                  c >= smaller && c <= greater

toNumber :: [Int] -> Int
toNumber xs = toNumber' (reverse xs)
  where 
    toNumber' [x]     = x
    toNumber' (x: xs) = x + 10 * toNumber' xs 
    
sliding :: Int -> Int -> [a] -> [[a]]
sliding width step xs
    | len < width = []
    | otherwise   = take width xs : sliding width step (drop step xs)
         where
            len = length xs    

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

-- while queue not empty: take bottom el, eval with utilityFunction, if finished exit
-- otherwise get children, filter on not visited, add then to visited and to front of queue
-- Returns the reversed path with the matched element at the top. Or nothing if not found.
bfs :: Ord a => a -> (a -> [a]) -> (a -> Bool) -> Maybe [a]
bfs root getChildren matchFunction =
    let queue = D.pushFront (D.empty :: D.BankersDequeue [a]) [root]
        visited = S.singleton root
        (_, _, result) = head $ dropWhile (\(q,_,r) -> not (null q) && isNothing r) $ iterate nextCandidate (queue, visited, Nothing) in
    result
    where nextCandidate (q, v, _) = let Just (path, poppedQ) = D.popBack q
                                        candidate = head path in
                                    if matchFunction candidate then (poppedQ, v, Just path)
                                    else let children = filter (`S.notMember` v) (getChildren candidate)
                                             newVisited = S.union v (S.fromList children)
                                             nextQueue = foldl (\newQ c -> D.pushFront newQ (c : path)) poppedQ children in
                                         (nextQueue, newVisited, Nothing)