module Day21 where

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

collectOutputs :: Input -> Program -> [Output]
collectOutputs input program = case execute input program of
    Halted                        -> []
    (WaitForOutput prog inp outp) -> outp : collectOutputs inp prog

commandsToInput :: [String] -> [Integer]
commandsToInput cs = cs >>= (\s -> fromIntegral <$> (fmap ord s ++ [10]))

part1 :: IO ()
part1 = do
    content <- readFile "resources/day21.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let buffer = replicate 10000 0
    let opArray = array (0, length numbers + length buffer  - 1) $ zip [0..] (numbers ++ buffer)
    let program = (0,0,opArray)
    -- Jump if any hole is coming and if there is a landing spot
    let commands = ["NOT A T", "NOT B J", "OR J T", "NOT C J", "OR T J", "AND D J", "WALK"]
    let input = commandsToInput commands
    let gridOrResult = fromInteger <$> collectOutputs input program
    if last gridOrResult < 256 then
      putStrLn $ fmap chr gridOrResult
    else
      putStrLn $ "Solution: " ++ fmap chr (init gridOrResult) ++ show (last gridOrResult)

part2 :: IO ()
part2 = do
    content <- readFile "resources/day21.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let buffer = replicate 10000 0
    let opArray = array (0, length numbers + length buffer  - 1) $ zip [0..] (numbers ++ buffer)
    let program = (0,0,opArray)
    -- Jump if there is any hole coming but only if there is a landing spot that is either not followed by a hole or has a next landing spot.
    let commands = ["NOT A T", "NOT B J", "OR J T", "NOT C J", "OR T J", "AND D J", "NOT E T", "NOT T T", "OR H T", "AND T J", "RUN"]
    let input = commandsToInput commands
    let gridOrResult = fromInteger <$> collectOutputs input program
    if last gridOrResult < 256 then
      putStrLn $ fmap chr gridOrResult
    else
      putStrLn $ "Solution: " ++ fmap chr (init gridOrResult) ++ show (last gridOrResult)


