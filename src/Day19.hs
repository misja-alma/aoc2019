module Day19 where

import Data.List.Split
import Data.Array
import Data.List
import Data.Function
import Data.Char
import Utils
import Data.Maybe
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

collectOutput :: Input -> Program -> Int
collectOutput input program = case execute input program of
    (WaitForOutput prog inp outp) -> fromInteger outp

type Grid = Array (Index, Index) Int

tupleToArray :: (Int, Int) -> [Integer]
tupleToArray (a, b) = [fromIntegral a, fromIntegral b]

gridWidth = 50
gridHeight = 50

collectGrid :: Program -> Grid
collectGrid prog = let coords = [(x,y) | x <- [0..gridWidth - 1], y <- [0..gridHeight - 1]]
                       initialGrid = listArray ((0,0), (gridWidth - 1,gridHeight - 1)) (replicate (gridWidth * gridHeight) 0)
                   in foldl writeCell initialGrid coords
                      where writeCell gr xy =
                                let value = collectOutput (tupleToArray xy) prog
                                in gr // [(xy, value)]

toChar :: Int -> Char
toChar 1 = '#'
toChar _ = '.'

showLine :: Grid -> Int -> Int -> Int -> String
showLine grid startX endX y = fmap (\x -> toChar $ grid ! (x, y)) [startX .. endX]

display :: Grid -> String
display grid = let startY = 0
                   endY = gridHeight - 1
                   startX = 0
                   endX = gridWidth - 1 in
               unlines $ fmap (showLine grid startX endX) [startY .. endY]

findFirstBeam :: Grid -> Int -> Int
findFirstBeam grid y = fromJust $ find (\x -> grid ! (x,y) == 1) [0..]

part1 :: IO ()
part1 = do
    content <- readFile "resources/day19.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let buffer = replicate 10000 0
    let opArray = array (0, length numbers + length buffer  - 1) $ zip [0..] (numbers ++ buffer)
    let grid = collectGrid (0,0,opArray)
    putStrLn $ display grid
    let starts = fmap (\y -> (findFirstBeam grid y, y)) [5..gridHeight-1]
    putStrLn $ unlines (fmap show starts)
    let solution = count (\(_, v) -> v == 1) (assocs grid)
    putStrLn $ "Solution: " ++ show solution

widthAtHeight :: Int -> Int -- This is wrong! We need an xEndAtHeight as well ..
widthAtHeight y = y `div` 5

xStartAtHeight :: Int -> Int
xStartAtHeight y = 1 + 33 * y `div` 43

firstFit :: Int -> (Int, Int)
firstFit width = let widthAtTop = width + width `div` 2
                     yBottom = widthAtTop * 5 + (widthAtTop - 1)
                     bottomXYs = fmap (\y -> (xStartAtHeight y, y)) [yBottom..]
                 in fromJust $ find (\(x, y) -> xStartAtHeight (y-99) <= x) bottomXYs

findFirstDot :: Program -> (Int, Int) -> Int
findFirstDot prog (startX,y) =
    let valAtPos = collectOutput [fromIntegral startX, fromIntegral y] prog
    in if valAtPos == 0 then
           fromJust $ find (\x -> collectOutput [fromIntegral x, fromIntegral y] prog == 1) [startX+1..]
    else let res = fromJust $ find (\x -> collectOutput [fromIntegral x, fromIntegral y] prog == 0) [startX-1,(startX-2)..0]
         in res + 1

widthAt :: Program -> (Int, Int) -> Int
widthAt prog (startX, y) =
    let res = fromJust $ find (\x -> collectOutput [fromIntegral x, fromIntegral y] prog == 0) [startX..]
    in res - startX

part2 :: IO ()
part2 = do
    content <- readFile "resources/day19.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let buffer = replicate 10000 0
    let opArray = array (0, length numbers + length buffer  - 1) $ zip [0..] (numbers ++ buffer)
    let program = (0,0, opArray)
    let (x', y') = firstFit 100
    let x = x' + 146 -- Found by manual binary search :)
    let y = y' + 146
    let firstX = findFirstDot program (x,y)
    putStrLn $ "Solution: " ++ show (firstX * 10000 + (y-99))
    let w = widthAt program (firstX, y-99)
    putStrLn $ "First x: " ++ show firstX ++ " last y: " ++ show y ++ " width at top: " ++ show w
    -- check that square really fits
--    let coords = [(x,y)| x <- [firstX..firstX+99], y <-[y-99..y]]
--    let correct = all (\pos -> collectOutput (tupleToArray pos) program == 1) coords
--    putStrLn $ "Correct: " ++ show correct

