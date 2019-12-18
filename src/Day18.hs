module Day18 where

import Debug.Trace

import Data.Array
import qualified Data.HashSet as S
import Data.Maybe
import Data.List
import Data.Char
import Data.Hashable

import qualified Data.Dequeue as D
import qualified Heap as H

type Grid = Array (Int, Int) Char

gridWidth = 100
gridHeight = 100

rowToCells :: (String, Int) -> [(Int, Int, Char)]
rowToCells (cs, y) = (\(c, x) -> (x,y,c)) <$> zip cs [0..]

drawItem :: Grid -> (Int, Int, Char) -> Grid
drawItem grid (x,y,c) = grid // [((x,y), c)]

showLine :: Grid -> Int -> Int -> Int -> String
showLine grid startX endX y = fmap (\x -> grid ! (x, y)) [startX .. endX]

display :: Grid -> String
display grid = let startY = 0
                   endY = gridHeight - 1
                   startX = 0
                   endX = gridWidth - 1 in
               unlines $ fmap (showLine grid startX endX) [startY .. endY]

findPos :: Char -> Grid -> (Int, Int)
findPos o grid = let coords = [(x,y)| x<-[0..gridWidth-1], y<-[0..gridHeight-1]] in
                 fromJust $ find (\xy -> grid ! xy == o) coords

data CollectState = Collecting Int (Int, Int) (S.Set Char)

instance Show CollectState where
    show (Collecting pos _ keys) = show pos ++ " -> " ++ S.elems keys

instance Eq CollectState where
    (Collecting _ pos1 k1) == (Collecting _ pos2 k2) = pos1 == pos2 && k1 == k2

instance Hashable CollectState where
     hashWithSalt salt (Collecting _ pos1 k1) = salt + (hash pos1) * 65537 + hash (S.elems k1)

instance Ord CollectState where
    compare (Collecting _ pos1 k1) (Collecting _ pos2 k2) =
        case compare pos1 pos2 of
            EQ    -> compare k1 k2
            other -> other

isKey :: Char -> Bool
isKey c = isAlpha c && isLower c

isDoor :: Char -> Bool
isDoor c = isAlpha c && isUpper c

getPossiblePositions :: Grid -> (Int, Int) -> [(Int, Int)]
getPossiblePositions grid (x,y) = (if x > 0 then [(x-1,y)] else []) ++ (if y > 0 then [(x,y-1)] else []) ++ [(x+1,y),(x,y+1)]

toNewState :: Grid -> CollectState -> (Int, Int) -> Maybe CollectState
toNewState grid (Collecting lnt (x,y) keys) pos =
    let valAtPos = grid ! pos in
    if valAtPos == '#' then Nothing
    else if isKey valAtPos then Just $ Collecting (lnt + 1) pos (S.insert valAtPos keys)
         else if isDoor valAtPos then if S.member (toLower valAtPos) keys then
                                           Just $ Collecting (lnt + 1) pos keys
                                      else Nothing
              else Just $ Collecting (lnt + 1) pos keys

possibleMoves :: Grid -> CollectState -> [CollectState]
possibleMoves grid st@(Collecting lnt (x,y) keys) =
    let possiblePositions = getPossiblePositions grid (x,y)
        possibleStates = (toNewState grid st) <$> possiblePositions
    in catMaybes possibleStates

part1 :: IO()
part1 = do
    content <- readFile "resources/day18.txt"
    let cells = zip (lines content) [0..] >>= rowToCells
    let initialGrid = listArray ((0,0), (gridWidth - 1,gridHeight - 1)) (replicate (gridWidth * gridHeight) '.')
    let grid = foldl drawItem initialGrid cells
    putStrLn $ display grid
    let initialPos = findPos '@' grid
    let initialState = Collecting 0 initialPos S.empty
    let result = bfs' initialState (possibleMoves grid) (\(Collecting _ _ keys) -> S.size keys == 26)
    case result of
       Just (Collecting shortestPath _ _) ->
         putStrLn $ "Solution: " ++ show shortestPath
       Nothing -> putStrLn "No solution found!"

bfs' :: CollectState -> (CollectState -> [CollectState]) -> (CollectState -> Bool) -> Maybe CollectState
bfs' root getChildren matchFunction =
    let queue = D.pushFront (D.empty :: D.BankersDequeue CollectState) root
        visited = S.singleton root
        (_, _, result) = head $ dropWhile (\(q,_,r) -> not (null q) && isNothing r) $ iterate nextCandidate (queue, visited, Nothing) in
    result
    where nextCandidate (q, v, _) = let Just (candidate, poppedQ) = D.popBack q in
                                    if matchFunction candidate then (poppedQ, v, Just candidate)
                                    else let children = filter (`S.notMember` v) (getChildren candidate)
                                             newVisited = S.union v (S.fromList children)
                                             nextQueue = foldl D.pushFront poppedQ children in
                                         (nextQueue, newVisited, Nothing)

data TotalState = TotalState Int [(Int, Int)] (S.Set Char)

instance Show TotalState where
    show (TotalState len ps keys) = show len ++ " " ++ unwords (fmap show ps) ++ " -> " ++ S.elems keys

-- Note that eq is different than ord
instance Eq TotalState where
    (TotalState len1 ps1 k1) == (TotalState len2 ps2 k2) = ps1 == ps2 && k1 == k2 -- Note that we disregard len in purpose

instance Hashable TotalState where
    hashWithSalt salt (TotalState ln ps k1) = salt + hash [hash $ fmap (\(x,y) -> x + y * gridWidth) ps, hash (S.elems k1)] -- Note that we disregard len in purpose

instance Ord TotalState where
    compare (TotalState len1 ps1 k1) (TotalState len2 ps2 k2) =
        case compare len2 len1 of      -- smaller is better
            EQ    -> compare (S.size k1) (S.size k2) -- bigger is better
            other -> other

findAllPos :: Char -> Grid -> [(Int, Int)]
findAllPos o grid = let coords = [(x,y)| x<-[0..gridWidth-1], y<-[0..gridHeight-1]] in
                 filter (\xy -> grid ! xy == o) coords

nrOfKeys :: TotalState -> Int
nrOfKeys (TotalState _ _ keys) = S.size keys

nrOfSteps :: TotalState -> Int
nrOfSteps (TotalState steps _ _) = steps

positions :: TotalState -> [(Int, Int)]
positions (TotalState _ ps _) = ps

finished :: TotalState -> Bool
finished states = 26 == nrOfKeys states

-- move one robot at the time
possibleMoves2 :: Grid -> TotalState -> [TotalState]
possibleMoves2 grid state = concatMap (possibleMoves' grid state) (positions state)

isNotBlocked :: Grid -> S.Set Char -> (Int, Int) -> Bool
isNotBlocked grid keys pos =
    let valAtPos = grid ! pos in
    valAtPos /= '#' && (not (isDoor valAtPos) || S.member (toLower valAtPos) keys)

reachableNeighbours :: Grid -> S.Set Char -> ((Int, Int), Int) -> [((Int, Int), Int)]
reachableNeighbours grid keys ((x,y), ln) =
    let reallyReachable = filter (isNotBlocked grid keys) $ getPossiblePositions grid (x,y)
    in fmap (\p -> (p, ln + 1)) reallyReachable

possibleMoves' :: Grid -> TotalState -> (Int, Int) -> [TotalState]
possibleMoves' grid state@(TotalState ln ps keys) pos@(x,y) =
    let others = filter (/= pos) ps
        possibleNewKeys = bfs3 grid keys ((x,y), ln) (reachableNeighbours grid keys)
    in toNewState' keys others <$> possibleNewKeys

type PosWithDist = ((Int, Int), Int)

-- Assume position is at new key
toNewState' :: S.Set Char -> [(Int, Int)] -> (PosWithDist, Char) -> TotalState
toNewState' keys others ((pos, dist), keyAtPos) =
    TotalState dist (pos: others) (S.insert keyAtPos keys)

inResults :: [(PosWithDist,  Char)] -> Char -> Bool
inResults rs c = c `elem` fmap snd rs

-- returns pair of unreached star positions with their path lengths. Note that the algo does not look further on the same path once it reaches a new star.
-- input = grid, orig. state, orig pos with orig pathlength
bfs3 :: Grid -> S.Set Char -> PosWithDist -> (PosWithDist -> [PosWithDist]) -> [(PosWithDist, Char)]
bfs3 grid keys root getChildren =
    let queue = D.pushFront (D.empty :: D.BankersDequeue PosWithDist) root
        visited = S.singleton (fst root)
        (_, _, result) = head $ dropWhile (\(q,_,r) -> not (null q)) $ iterate nextCandidate (queue, visited, []) in
    result
    where nextCandidate (q, v, rs) =
                                      let Just (candidate, poppedQ) = D.popBack q
                                          (pos, _) = candidate
                                          c = grid ! pos in
                                      if (isKey c) && not (S.member c keys) && not (inResults rs c) then
                                         (poppedQ, v, (candidate, c) : rs) -- We don't continue. Note that this means that some other key on the same path now could be added on
                                                                      -- an inferior path. But the pq should filter this out quickly.
                                      else
                                         let children = filter (\cp -> S.notMember (fst cp) v) (getChildren candidate)
                                             newVisited = S.union v (S.fromList $ fmap fst children)
                                             nextQueue = foldl D.pushFront poppedQ children in
                                         (nextQueue, newVisited, rs)

part2 :: IO()
part2 = do
    content <- readFile "resources/day18_2.txt"
    let cells = zip (lines content) [0..] >>= rowToCells
    let initialGrid = listArray ((0,0), (gridWidth - 1,gridHeight - 1)) (replicate (gridWidth * gridHeight) '.')
    let grid = foldl drawItem initialGrid cells
    let initialPositions = findAllPos '@' grid
    let initialState = TotalState 0 initialPositions S.empty
    let result = aStarSearch initialState (possibleMoves2 grid) finished
    case result of
       Just state ->
         let solution = nrOfSteps state in
         putStrLn $ "Solution: " ++ show solution
       Nothing -> putStrLn "No solution found!"

insertBatch :: Ord a => [a] -> H.Heap a -> H.Heap a
insertBatch xs h = foldl (\h' x -> H.insert x h') h xs

aStarSearch :: TotalState -> (TotalState -> [TotalState]) -> (TotalState -> Bool) -> Maybe TotalState
aStarSearch root getChildren matchFunction =
    let queue = H.singleton root
        visited = S.singleton root
        (_, _, result) = head $ dropWhile (\(q,_,r) -> not (H.isEmpty q) && isNothing r) $ iterate nextCandidate (queue, visited, Nothing) in
    result
    where nextCandidate (q, v, _) = let (candidate, poppedQ) = H.deleteMax q in
                                    if matchFunction candidate then (poppedQ, v, Just candidate)
                                    else let children = filter (`S.notMember` v) (getChildren candidate)
                                             newVisited = S.union v (S.fromList children)
                                             nextQueue = insertBatch children poppedQ in
                                         (nextQueue, newVisited, Nothing)