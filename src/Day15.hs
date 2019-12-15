{-# LANGUAGE NamedFieldPuns #-}

module Day15 where

import Data.List.Split
import Data.Array
import Data.List
import Data.Function
import Utils
import Data.Maybe

import qualified Data.Dequeue as D
import qualified Data.Set as S

import Debug.Trace

type Index = Int
type Base = Index
type Program = (Index, Base, Array Index Integer)
type Input = [Integer]
type Output = Integer
data ProgramState = Running Program Input | WaitForOutput Program Input Output | Halted
type Operation = Input -> Program -> ProgramState
type Opcode = Integer

peek :: Array Index Integer -> Index -> Base -> Integer
peek ar pt base = ar ! (fromInteger (ar ! pt) + base)

getMode :: Opcode -> Int -> Integer
getMode mode offset = case offset of
    1 -> (mode `div` 100) `mod` 10
    2 -> (mode `div` 1000) `mod` 10
    3 -> (mode `div` 10000) `mod` 10

readParam :: Program -> Index -> Opcode -> Integer
readParam (ip, base, ar) offset mode = case getMode mode offset of
    0 -> peek ar (ip + offset) 0
    1 -> ar ! (ip + offset)
    2 -> peek ar (ip + offset) base

write :: Program -> Index -> Opcode -> Integer -> Array Index Integer
write (ip, base, ar) offset mode value = case getMode mode offset of
    2 -> ar // [(base + fromInteger (ar ! (ip + offset)), value)]
    _ -> ar // [(fromInteger $ ar ! (ip + offset), value)]

exit :: Operation
exit _ _ = Halted

add :: Opcode -> Operation
add opcode inp prog@(ip, base, ar) =
    let p1 = readParam prog 1 opcode
        p2 = readParam prog 2 opcode
        sum = p1 + p2
        newAr = write prog 3 opcode sum in
    Running (ip + 4, base, newAr) inp

mul :: Opcode -> Operation
mul opcode inp prog@(ip, base, ar) =
    let p1 = readParam prog 1 opcode
        p2 = readParam prog 2 opcode
        product = p1 * p2
        newAr = write prog 3 opcode product in
    Running (ip + 4, base, newAr) inp

input :: Opcode -> Operation
input opcode inp prog@(ip, base, ar) =
    let newAr = write prog 1 opcode (head inp) in
    Running (ip + 2, base, newAr) (tail inp) -- TODO or maybe return WaitingForInput if no input available

output :: Opcode -> Operation
output opcode inp prog@(ip, base, ar) =
   let p1 = readParam prog 1 opcode in
   WaitForOutput (ip + 2, base, ar) inp p1

jumpIfTrue :: Opcode -> Operation
jumpIfTrue opcode inp prog@(ip, base, ar) =
      let p1 = readParam prog 1 opcode
          p2 = readParam prog 2 opcode in
      if p1 /= 0 then Running (fromInteger p2, base, ar) inp else Running (ip + 3, base, ar) inp

jumpIfFalse :: Opcode -> Operation
jumpIfFalse opcode inp prog@(ip, base, ar) =
      let p1 = readParam prog 1 opcode
          p2 = readParam prog 2 opcode in
      if p1 == 0 then Running (fromInteger p2, base, ar) inp else Running (ip + 3, base, ar) inp

lessThan :: Opcode -> Operation
lessThan opcode inp prog@(ip, base, ar) =
      let p1 = readParam prog 1 opcode
          p2 = readParam prog 2 opcode
          newAr = if p1 < p2 then write prog 3 opcode 1 else write prog 3 opcode 0 in
      Running (ip + 4, base, newAr) inp

equals :: Opcode -> Operation
equals opcode inp prog@(ip, base, ar) =
      let p1 = readParam prog 1 opcode
          p2 = readParam prog 2 opcode
          newAr = if p1 == p2 then write prog 3 opcode 1 else write prog 3 opcode 0 in
      Running (ip + 4, base, newAr) inp

setBase :: Opcode -> Operation
setBase opcode inp prog@(ip, base, ar) =
    let p1 = readParam prog 1 opcode in
    Running (ip + 2, base + fromInteger p1, ar) inp

execute :: Input -> Program -> ProgramState
execute input prog@(pc, base, ar) =
    let instr = ar ! pc
        operation = translate instr
        result = operation input prog in
    case result of
        Running newProg newInput -> execute newInput newProg
        outputOrHalted           -> outputOrHalted

translate :: Opcode -> Operation
translate opcode =
    case opcode `mod` 100 of
      99 -> exit
      1  -> add opcode
      2  -> mul opcode
      3  -> input opcode
      4  -> output opcode
      5  -> jumpIfTrue opcode
      6  -> jumpIfFalse opcode
      7  -> lessThan opcode
      8  -> equals opcode
      9  -> setBase opcode

collectOutput :: Input -> Program -> (Program, Int)
collectOutput input program = case execute input program of
    (WaitForOutput prog inp outp) -> (prog, fromInteger outp)

data DroidState = DroidState (Int, Int) Int Program

instance Show DroidState where
    show (DroidState pos1 o _) = show pos1 ++ " -> " ++ show o

instance Eq DroidState where
    (DroidState pos1 _ _) == (DroidState pos2 _ _) = pos1 == pos2

instance Ord DroidState where
    compare (DroidState pos1 _ _) (DroidState pos2 _ _) = compare pos1 pos2

possibleMoves :: DroidState -> [DroidState]
possibleMoves (DroidState (x,y) currentOutput prog) =
    if currentOutput == 0 then [] else
      let north = ((x, y-1), collectOutput [1] prog)
          south = ((x, y+1), collectOutput [2] prog)
          west = ((x-1, y), collectOutput [3] prog)
          east = ((x+1, y), collectOutput [4] prog) in
      toDroidState <$> [north, south, west, east]
          where toDroidState (a, (b, c)) = DroidState a c b

part1 :: IO ()
part1 = do
    content <- readFile "resources/day15.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let buffer = replicate 1000 0
    let opArray = array (0, length numbers + length buffer  - 1) $ zip [0..] (numbers ++ buffer)
    let program = (0,0,opArray)
    let initialState = DroidState (0,0) 1 program
    let (_, _, result) = bfs' initialState possibleMoves (\(DroidState _ result _) -> result == 2)
    case result of
       Just shortestPath ->
         let solution = length shortestPath - 1 in
         putStrLn $ "Solution: " ++ show solution
       Nothing -> putStrLn "No solution found!"

bfs' :: DroidState -> (DroidState -> [DroidState]) -> (DroidState -> Bool) -> (Int, S.Set DroidState, Maybe [DroidState])
bfs' root getChildren matchFunction =
    let queue = D.pushFront (D.empty :: D.BankersDequeue [DroidState]) [root]
        visited = S.singleton root
        maxPath = 0
        (mp, _, finalVisited, result) = head $ dropWhile (\(_,q,_,r) -> not (null q) && isNothing r) $ iterate nextCandidate (maxPath, queue, visited, Nothing) in
    (mp, finalVisited, result)
    where nextCandidate (mp, q, v, _) = let Just (path, poppedQ) = D.popBack q
                                            candidate = head path in
                                        if matchFunction candidate then (mp + 1, poppedQ, v, Just path)
                                        else let children = filter (`S.notMember` v) (getChildren candidate)
                                                 newVisited = S.union v (S.fromList children)
                                                 nextQueue = foldl (\newQ c -> D.pushFront newQ (c : path)) poppedQ children in
                                             (length path, nextQueue, newVisited, Nothing)

type Grid = Array (Index, Index) Int
gridWidth = 100
gridHeight = 100

drawItem :: Grid -> DroidState -> Grid
drawItem grid (DroidState (x,y) o _) = grid // [((x,y), o)]

notOccupied :: Grid -> (Int, Int) -> Bool
notOccupied grid pos = grid ! pos == 1

showPixel :: Int -> Char
showPixel 0  = '#'
showPixel 1  = ' '
showPixel 2  = 'X'

showLine :: Grid -> Int -> Int -> Int -> String
showLine grid startX endX y = fmap (\x -> showPixel $ grid ! (x, y)) [startX .. endX]

display :: Grid -> String
display grid = let startY = 0
                   endY = gridHeight - 1
                   startX = 0
                   endX = gridWidth - 1 in
               unlines $ fmap (showLine grid startX endX) [startY .. endY]

part2 :: IO ()
part2 = do
    content <- readFile "resources/day15.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let buffer = replicate 1000 0
    let opArray = array (0, length numbers + length buffer  - 1) $ zip [0..] (numbers ++ buffer)
    let program = (0,0,opArray)
    let initialState = DroidState (50,50) 1 program
    let (pl, states, path) = bfs' initialState possibleMoves (\(DroidState _ result _) -> result == 42)
    -- TODO we need a bfs here that keeps searching!
    let initialGrid = listArray ((0,0), (gridWidth - 1,gridHeight - 1)) (replicate (gridWidth * gridHeight) 0)
    let grid = foldl drawItem initialGrid states
    putStrLn $ display grid
--    let DroidState op _ _ = head $ fromJust path
--    putStrLn $ "org pos: " ++ show op
    let finalPos = (36,66)
    putStrLn $ show finalPos
    print pl -- TODO seems to be 1 too high, because of initial state
    -- start at final pos. Children are all not blocked grid elements. Search function is some impossible func.
    let initialState2 = finalPos
    let posMoves (x,y) = filter (notOccupied grid) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
    let successFunc (x,y) = False
    let (pl2, _, _) = bfs2 initialState2 posMoves successFunc
    putStrLn $ "Solution: " ++ show (pl2 - 1)

-- TODO check why parametric polymorphism doesn't seem to work here
bfs2 :: (Int, Int) -> ((Int, Int) -> [(Int, Int)]) -> ((Int, Int) -> Bool) -> (Int, S.Set (Int, Int), Maybe [(Int, Int)])
bfs2 root getChildren matchFunction =
    let queue = D.pushFront (D.empty :: D.BankersDequeue [(Int, Int)]) [root]
        visited = S.singleton root
        maxPath = 0
        (mp, _, finalVisited, result) = head $ dropWhile (\(_,q,_,r) -> not (null q) && isNothing r) $ iterate nextCandidate (maxPath, queue, visited, Nothing) in
    (mp, finalVisited, result)
    where nextCandidate (mp, q, v, _) = let Just (path, poppedQ) = D.popBack q
                                            candidate = head path in
                                        if matchFunction candidate then (mp + 1, poppedQ, v, Just path)
                                        else let children = filter (`S.notMember` v) (getChildren candidate)
                                                 newVisited = S.union v (S.fromList children)
                                                 nextQueue = foldl (\newQ c -> D.pushFront newQ (c : path)) poppedQ children in
                                             (length path, nextQueue, newVisited, Nothing)