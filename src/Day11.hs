{-# LANGUAGE NamedFieldPuns #-}

module Day11 where

import Data.List.Split
import Data.Array
import Data.List
import Data.Function
import Utils

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

type Grid = Array (Index, Index) Int
data RobotState = RobotState { grid :: Grid, position :: (Int, Int), direction :: DIRECTION, program :: Program, finished :: Bool }
data DIRECTION = UP | LEFT | DOWN | RIGHT deriving (Enum, Eq, Ord)

updateDirection :: Integer -> DIRECTION -> DIRECTION  -- turn 0 is left, 1 is right
updateDirection turn direction = let adjustment = if turn == 0 then 1 else -1 in toEnum $ (fromEnum direction + adjustment + 4) `rem` 4

move :: (Int, Int) -> DIRECTION -> (Int, Int)
move (x,y) direction = case direction of
    UP    -> (x, y - 1)
    LEFT  -> (x - 1, y)
    DOWN  -> (x, y + 1)
    RIGHT -> (x + 1, y)

moveRobot :: RobotState -> RobotState
moveRobot (RobotState grid pos direction program _) =
    let value = grid ! pos
        corrected = if value == -1 then 0 else value in
    case execute [fromIntegral corrected] program of
        Halted                        -> RobotState grid pos direction program True
        (WaitForOutput prog1 _ color) ->
            let WaitForOutput prog2 _ turn = execute [] prog1
                newDirection = updateDirection turn direction
                newPos = move pos newDirection in
              RobotState (grid // [(pos, fromInteger color)]) newPos newDirection prog2 False

gridWidth = 200
gridHeight = 200

part1 :: IO ()
part1 = do
    content <- readFile "resources/day11.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let buffer = replicate 1000 0
    let opArray = array (0, length numbers + length buffer  - 1) $ zip [0..] (numbers ++ buffer)
        -- Initialise grid with -1, 0 means black, 1 white
    let initialGrid = listArray ((0,0), (gridWidth - 1,gridHeight - 1)) (replicate (gridWidth * gridHeight) (-1))
    let startingPos = (gridWidth `div` 2,gridHeight `div` 2)
    let startingProg = (0,0,opArray)
    let startingDirection = UP
    let startingState = RobotState initialGrid startingPos startingDirection startingProg False
    let states = iterate moveRobot startingState
    let finalState = head $ dropWhile (not . finished) states
    let solution = count (/= -1) $ elems $ grid finalState
    putStrLn $ "Solution: " ++ show solution

showPixel :: Int -> Char
showPixel (-1) = ' '
showPixel 0  = ' '
showPixel 1  = '#'

showLine :: Grid -> Int -> Int -> Int -> String
showLine grid startX endX y = fmap (\x -> showPixel $ grid ! (x, y)) [startX .. endX]

display :: Grid -> String
display grid = let startY = 50
                   endY = 150
                   startX = 80
                   endX = 190 in
               unlines $ fmap (showLine grid startX endX) [startY .. endY]

part2 :: IO ()
part2 = do
    content <- readFile "resources/day11.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let buffer = replicate 1000 0
    let opArray = array (0, length numbers + length buffer  - 1) $ zip [0..] (numbers ++ buffer)
    let initialGrid = listArray ((0,0), (gridWidth - 1,gridHeight - 1)) (replicate (gridWidth * gridHeight) (-1))
    let startingPos = (gridWidth `div` 2,gridHeight `div` 2)
    let startingProg = (0,0,opArray)
    let startingDirection = UP
    -- start at white panel
    let startingState = RobotState (initialGrid // [(startingPos, 1)]) startingPos startingDirection startingProg False
    let states = iterate moveRobot startingState
    let finalState = head $ dropWhile (not . finished) states
    putStrLn $ "Solution: " ++ display (grid finalState)





