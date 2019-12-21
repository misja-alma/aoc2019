{-# LANGUAGE NamedFieldPuns #-}

module Day20 where

import Data.List.Split
import Data.Array
import Data.List
import Data.Function
import Utils
import Data.Maybe
import Data.Char
import qualified Data.Map.Strict as M

import qualified Deque.Strict as D
import qualified Data.Set as S
import qualified Data.Heap as H

import Debug.Trace

findAllGates :: Grid -> M.Map String [(Int, Int)]
findAllGates grid =  let positions = filter (isOnPath grid) [(x,y)| x<-[0..gridWidth-1], y<-[0..gridHeight-1]]
                         maybes = fmap (\pos -> fmap (\g -> (g, [pos])) (nextToGate grid pos)) positions
                     in M.fromListWith (++) (catMaybes maybes)

getGateOnPos :: Grid -> (Int, Int) -> Maybe String
getGateOnPos grid pos@(x, y) =
    let valAtPos = grid ! pos in
        if isAlpha valAtPos then
            -- Read other letter, mind orientation
            let neighbours = validMoves grid pos
                letterPos@(lx, ly) = head $ filter (\p -> isAlpha $ grid ! p) neighbours
                letter = grid ! letterPos in
            if lx < x || ly < y then Just [letter, valAtPos] else Just [valAtPos, letter]
        else Nothing

nextToGate :: Grid -> (Int, Int) -> Maybe String
nextToGate grid pos = listToMaybe $ mapMaybe (getGateOnPos grid) (validMoves grid pos)

notNeighbours :: (Int, Int) -> (Int, Int) -> Bool
notNeighbours (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1) > 1

-- If on a gate letter, transport to pos next to corresponding other gate
transportPortal :: Grid -> M.Map String [(Int, Int)] -> (Int, Int) -> (Int, Int)
transportPortal grid gates pos = case getGateOnPos grid pos of
                                   Just gate -> let allGates = gates M.! gate
                                                -- don't transport through ZZ or AA
                                                in if length allGates == 1 then pos else head $ filter (notNeighbours pos) allGates
                                   Nothing   -> pos

isOnPath :: Grid -> (Int, Int) -> Bool
isOnPath grid (x, y) = x >= 0 && y >= 0 && x < gridWidth && y <= gridHeight && (grid ! (x,y) == '.')

isValidPos :: Grid -> (Int, Int) -> Bool
isValidPos grid (x, y) = x >= 0 && y >= 0 && x < gridWidth && y <= gridHeight && (grid ! (x,y) /= '#') && (grid ! (x,y) /= ' ')

validMoves :: Grid -> (Int, Int) -> [(Int, Int)]
validMoves grid (x,y) = filter (isValidPos grid) [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]

possibleMoves :: Grid -> M.Map String [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
possibleMoves grid gates pos  =
    let pms = validMoves grid pos
        result = fmap (transportPortal grid gates) pms
    in result

rowToCells :: (String, Int) -> [(Int, Int, Char)]
rowToCells (cs, y) = (\(c, x) -> (x,y,c)) <$> zip cs [0..]

type Grid = Array (Int, Int) Char
gridWidth =  131
gridHeight = 131

part1 :: IO ()
part1 = do
    content <- readFile "resources/day20.txt"
    let cells = zip (lines content) [0..] >>= rowToCells
    let initialGrid = listArray ((0,0), (gridWidth - 1,gridHeight - 1)) (replicate (gridWidth * gridHeight) ' ')
    let grid = foldl (\g (x,y,v) -> g // [((x,y), v)]) initialGrid cells
    let gates = findAllGates grid
    let initialPos = head $ gates M.! "AA"
    putStrLn "Starting search .. "
    let result = bfs' initialPos (possibleMoves grid gates) (\p -> nextToGate grid p == Just "ZZ")
    case result of
       Just shortestPath ->
         let solution = length shortestPath - 1 in
         putStrLn $ "Solution: " ++ show solution
       Nothing -> putStrLn "No solution found!"

bfs' :: (Int, Int) -> ((Int, Int) -> [(Int, Int)]) -> ((Int, Int) -> Bool) -> Maybe [(Int, Int)]
bfs' root getChildren matchFunction =
    let queue = D.fromConsAndSnocLists [[root]] [[root]]
        visited = S.singleton root
        (_, finalVisited, result) = head $ dropWhile (\(q,_,r) -> not (null q) && isNothing r) $ iterate nextCandidate (queue, visited, Nothing) in
    result
    where nextCandidate (q, v, _) =   let Just (path, poppedQ) = D.unsnoc q
                                          candidate = head path in
                                      if matchFunction candidate then (poppedQ, v, Just path)
                                      else let children = filter (`S.notMember` v) (getChildren candidate)
                                               newVisited = S.union v (S.fromList children)
                                               nextQueue = foldl (\newQ c -> D.cons (c : path) newQ) poppedQ children in
                                           (nextQueue, newVisited, Nothing)

-- level starts at 0: move through inner gate increases level, outer gate decreases it.
-- gate ZZ needs to be reached at level 0.
-- So we need to know if a gate is inner or outer, and we need to keep a state consisting of both level and pos
-- Simple BFS was too slow, changing to a PQ ordered by length and level was already enough of a speedup.
part2 :: IO ()
part2 = do
    content <- readFile "resources/day20.txt"
    let cells = zip (lines content) [0..] >>= rowToCells
    let initialGrid = listArray ((0,0), (gridWidth - 1,gridHeight - 1)) (replicate (gridWidth * gridHeight) ' ')
    let grid = foldl (\g (x,y,v) -> g // [((x,y), v)]) initialGrid cells
    let gatesWithOffsets = findAllGates2 grid
    let (initialPos, gateOffset) = head $ gatesWithOffsets M.! "AA"
    let initialState = SearchState initialPos 0 0
    putStrLn  "Starting search .."
    let result = bfs2 initialState (possibleMoves2 grid gatesWithOffsets) (\(SearchState p l _) -> l == 0 && nextToGate grid p == Just "ZZ")
    case result of
       Just (SearchState _ _ solution) ->
         putStrLn $ "Solution: " ++ show solution
       Nothing -> putStrLn "No solution found!"

-- pos, level, length
data SearchState = SearchState (Int, Int) Int Int deriving Show

instance Ord SearchState where
    compare (SearchState _ level1 len1) (SearchState _ level2 len2) =
        case compare len1 len2 of                    -- smaller is better, min based
            EQ    -> compare level1 level2           -- smaller is better, min based
            other -> other

instance Eq SearchState where
    (SearchState pos1 level1 len1) == (SearchState pos2 level2 len2) = pos1 == pos2 && level1 == level2 && len1 == len2

isOuterGate :: (Int, Int) -> Bool
isOuterGate (x,y) = y == 2 || y == gridHeight - 3 || x == 2 || x == gridWidth - 3

addGateOffset :: (Int, Int) -> ((Int, Int), Int)
addGateOffset pos = if isOuterGate pos then (pos, -1) else (pos, 1)

findAllGates2 :: Grid -> M.Map String [((Int, Int), Int)]
findAllGates2 grid = let positions = filter (isOnPath grid) [(x,y)| x<-[0..gridWidth-1], y<-[0..gridHeight-1]]
                         maybes = fmap (\pos -> fmap (\g -> (g, [addGateOffset pos])) (nextToGate grid pos)) positions
                     in M.fromListWith (++) $ catMaybes maybes

validMoves2 :: Grid -> M.Map String [((Int, Int), Int)] -> ((Int, Int), Int) -> [((Int, Int), Int)]
validMoves2 grid gates ((x,y), level) = let normals = (\p -> (p, level)) <$> filter (isOnPath grid) [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]
                                            maybeTransport = case nextToGate grid (x,y) of
                                                Just gate -> let allGates = gates M.! gate
                                                              -- don't transport through AA or ZZ
                                                              in if length allGates == 1 then Nothing
                                                              else let (transportedPos, otherGateOffset) = head $ filter (\(p, _) -> p /= (x,y)) allGates
                                                                       newLevel = level - otherGateOffset in
                                                                   if newLevel >= 0 then Just (transportedPos, newLevel) else Nothing
                                                Nothing   -> Nothing
                                        in normals ++ maybeToList maybeTransport


possibleMoves2 :: Grid -> M.Map String [((Int, Int), Int)] -> SearchState -> [SearchState]
possibleMoves2 grid gates (SearchState pos level ln)  =
    let pms = validMoves2 grid gates (pos, level)
        result = fmap (\(pos, lv) -> SearchState pos lv (ln + 1)) pms
    in result

insertBatch :: Ord a => [a] -> H.Heap a -> H.Heap a
insertBatch xs h = foldl (\h' x -> H.insert x h') h xs

withoutLength :: SearchState -> ((Int, Int), Int)
withoutLength (SearchState pos level _) = (pos, level)

bfs2 :: SearchState -> (SearchState -> [SearchState]) -> (SearchState -> Bool) -> Maybe SearchState
bfs2 root getChildren matchFunction =
    let queue = H.singleton root
        visited = S.singleton $ withoutLength root
        (_, finalVisited, result) = head $ dropWhile (\(q,_,r) -> not (H.null q) && isNothing r) $ iterate nextCandidate (queue, visited, Nothing) in
    result
    where nextCandidate (q, v, _) =   let Just (candidate, poppedQ) = H.uncons q in
                                      if matchFunction candidate then (poppedQ, v, Just candidate)
                                      else let children = filter (\c -> withoutLength c `S.notMember` v) (getChildren candidate)
                                               newVisited = S.union v (S.fromList $ fmap withoutLength children)
                                               nextQueue = insertBatch children poppedQ in
                                           (nextQueue, newVisited, Nothing)