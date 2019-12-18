module Day17 where

import Data.List.Split
import Data.Array
import Data.List
import Data.Function
import Data.Char
import Utils
import Data.Maybe

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

execute :: Input -> Program -> ProgramState
execute input prog@(pc, base, ar) =
    let instr = ar ! pc
        operation = translate instr
        result = operation input prog in
    case result of
        Running newProg newInput -> execute newInput newProg
        outputOrHalted           -> outputOrHalted

collectOutputs :: Input -> Program -> [Output]
collectOutputs input program = case execute input program of
    Halted                        -> []
    (WaitForOutput prog inp outp) -> outp : collectOutputs inp prog

type Grid = Array (Index, Index) Int
gridWidth = 100
gridHeight = 100

drawCell :: (Grid, (Int, Int)) -> Integer -> (Grid, (Int, Int))
drawCell (grid, (x,y)) output =
  case (chr $ fromInteger output) of
      '#' -> (grid // [((x,y), 1)], (x+1, y))
      '.' -> (grid, (x+1, y))
      '\n' -> (grid, (0, y+1))
      '>' -> (grid // [((x,y), 10)], (x+1, y))
      '<' -> (grid // [((x,y), 11)], (x+1, y))
      '^' -> (grid // [((x,y), 12)], (x+1, y))
      unknown -> error ("Unknown char: " ++ show unknown)

isCrossing :: Grid -> ((Index, Index), Int) -> Bool
isCrossing grid ((x,y), val) =
    val == 1 &&
    (x > 0 && grid ! (x-1, y) == 1) &&
    (y > 0 && grid ! (x, y - 1) == 1) &&
    (y < gridHeight && grid ! (x, y + 1) == 1) &&
    (x < gridWidth && grid ! (x + 1, y) == 1)

part1 :: IO ()
part1 = do
    content <- readFile "resources/day17.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let buffer = replicate 10000 0
    let opArray = array (0, length numbers + length buffer  - 1) $ zip [0..] (numbers ++ buffer)
    let outputs = collectOutputs [] (0, 0, opArray)
    --putStrLn $ "Solution part1: " ++ (fmap (chr . fromInteger) outputs)
    let initialGrid = listArray ((0,0), (gridWidth - 1,gridHeight - 1)) (replicate (gridWidth * gridHeight) 0)
    let (grid, _) = foldl drawCell (initialGrid, (0,0)) outputs
    let solution = sum $ (\((x,y),_) -> x * y) <$> filter (isCrossing grid) (assocs grid)
    putStrLn $ "Solution: " ++ show solution

findPos :: Int -> Grid -> (Int, Int)
findPos o grid = let coords = [(x,y)| x<-[0..gridWidth-1], y<-[0..gridHeight-1]] in
                 fromJust $ find (\xy -> grid ! xy == o) coords

type Direction = Int
type Turn = (Char, Int)
data TurnState = Finished [Turn] | Walking (Int, Int) Direction [Turn]

showTurn :: Turn -> String
showTurn (c, d) = c : show d

isFinished :: TurnState -> Bool
isFinished (Finished _) = True
isFinished _            = False

-- direction: 0 = up, 1 = left, 2 = down, 3 = right
turnLeft :: Direction -> Direction
turnLeft direction = (direction + 1) `rem` 4

turnRight :: Direction -> Direction
turnRight direction = (direction - 1 + 4) `rem` 4

updatePos :: Direction -> Int -> (Int, Int) -> (Int, Int)
updatePos 0 distance (x,y) = (x, y - distance)
updatePos 1 distance (x,y) = (x - distance, y)
updatePos 2 distance (x,y) = (x, y + distance)
updatePos 3 distance (x,y) = (x + distance, y)

validPos :: (Int, Int) -> Bool
validPos (x, y) = x >= 0 && y >= 0

squaresAt :: Grid -> Direction -> (Int, Int) -> Int
squaresAt grid direction from =
    let route = iterate (updatePos direction 1) (updatePos direction 1 from)
    in length $ takeWhile (\p -> (validPos p) && (grid ! p == 1)) route

calcNextTurn :: Grid -> TurnState -> TurnState
calcNextTurn grid (Walking (x,y) direction ts) =
    -- Check for square at either left or right (this depends on the current direction). If none, then finished.
    -- then count nr squares in that direction
    let squaresLeft = squaresAt grid (turnLeft direction) (x,y)
        squaresRight = squaresAt grid (turnRight direction) (x,y) in
    if squaresRight > 0 then Walking (updatePos (turnRight direction) squaresRight (x,y)) (turnRight direction) (('R', squaresRight) : ts)
    else if squaresLeft > 0 then Walking (updatePos (turnLeft direction) squaresLeft (x,y)) (turnLeft direction) (('L', squaresLeft) : ts)
         else Finished ts

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

intToChars :: Int -> String
intToChars x = fmap intToDigit (digs x)

turnToChars :: (Char, [Int]) -> String
turnToChars (c, is) = c : (is >>= intToChars)

collectOutputs2 :: Input -> Program -> (Program, [Output])
collectOutputs2 input program = case execute input program of
    Halted                        -> (program, [])
    (WaitForOutput prog inp outp) -> let (newProg, outps) = collectOutputs2 inp prog in (newProg, outp : outps)

charsToStrings :: String -> [String]
charsToStrings s = (: []) <$> s

part2 :: IO ()
part2 = do
    content <- readFile "resources/day17.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let buffer = replicate 10000 0
    let opArray = array (0, length numbers + length buffer  - 1) $ zip [0..] (numbers ++ buffer)
    let outputs = collectOutputs [] (0, 0, opArray)
    --putStrLn $ "Solution part1:\n" ++ (fmap (chr . fromInteger) outputs)
    let initialGrid = listArray ((0,0), (gridWidth - 1,gridHeight - 1)) (replicate (gridWidth * gridHeight) 0)
    let (grid, _) = foldl drawCell (initialGrid, (0,0)) outputs
    let initialPos = findPos 12 grid
    putStrLn $ "Initial pos: " ++ show initialPos
    let initialDirection = 0 -- up
    let Finished turns = head $ dropWhile (not . isFinished) $ iterate (calcNextTurn grid) (Walking initialPos initialDirection [])
    putStrLn $ concatMap showTurn (reverse turns)
    let mr = ['A','B','A','B','A','C','B','C','A','C']
    let a = [('L',[6]),('R',[6,6]),('L',[6])]
    let b = [('R',[6,6]),('L',[5,5]),('L',[4]),('L',[6])]
    let c = [('L',[5,5]),('L',[5,5]),('L',[4]),('L',[6])]

    let mrInputs = ord <$> (intercalate "," (fmap (\x -> [x]) mr)) ++ ['\n']
    let aInputs = ord <$> (intercalate "," (charsToStrings (a >>= turnToChars))) ++ ['\n']
    let bInputs = ord <$> (intercalate "," (charsToStrings (b >>= turnToChars))) ++ ['\n']
    let cInputs = ord <$> (intercalate "," (charsToStrings (c >>= turnToChars))) ++ ['\n']
    let final = ord <$> ['n','\n']

    putStrLn $ unwords $ fmap show aInputs

    let allInputs = fromIntegral <$> (mrInputs ++ aInputs ++ bInputs ++ cInputs ++ final)

    let newProg = (0, 0, opArray // [(0, 2)])
    let finalOutput = collectOutputs allInputs newProg
    putStrLn (fmap (chr . fromInteger) (init finalOutput))

    putStrLn $ "Solution: " ++ show (last finalOutput)



