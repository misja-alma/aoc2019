module Day9 where

import Data.List.Split
import Data.Array
import Data.List
import Data.Function

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

-- A debug function can optionally print output based on the intermediary programState
-- or even stop at a conditional breakpoint with some new DebugState
type DebugFunction = Input -> Program -> IO Bool

printInstruction :: DebugFunction
printInstruction input prog@(pc, base, ar) = do
    let instr = ar ! pc
    let opString = showOperation instr prog
    putStrLn $ "Instruction: " ++ opString
    return False

debug :: Input -> Program -> DebugFunction -> IO ProgramState
debug input prog@(pc, base, ar) debugFunction = do
    debugResult <- debugFunction input prog -- TODO we should check if it is a breakpoint (i.e. True)
    let instr = ar ! pc
    let operation = translate instr
    let result = operation input prog
    case result of
        Running newProg newInput -> debug newInput newProg debugFunction
        outputOrHalted           -> return outputOrHalted

execute :: Input -> Program -> ProgramState
execute input prog@(pc, base, ar) =
    let instr = ar ! pc
        operation = translate instr
        result = operation input prog in
    case result of
        Running newProg newInput -> execute newInput newProg
        outputOrHalted           -> outputOrHalted

showParams :: Opcode -> Program -> Int -> String
showParams opcode prog nr = unwords $ fmap (showParam opcode prog) [1..nr]

-- TODO write param should not be fully read, clearer if just the address is shown
showParam :: Opcode -> Program -> Int -> String
showParam opcode prog i = show $ readParam prog i opcode

showOperation :: Opcode -> Program -> String
showOperation opcode prog =
    case opcode `mod` 100 of
      99 -> "exit"
      1  -> "add " ++ showParams opcode prog 3
      2  -> "mul " ++ showParams opcode prog 3
      3  -> "input " ++ showParams opcode prog 1
      4  -> "output " ++ showParams opcode prog 1
      5  -> "jumpIfTrue " ++ showParams opcode prog 2
      6  -> "jumpIfFalse " ++ showParams opcode prog 2
      7  -> "lessThan " ++ showParams opcode prog 3
      8  -> "equals " ++ showParams opcode prog 3
      9  -> "setBase " ++ showParams opcode prog 1

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

debugCollectOutputs :: Input -> Program -> DebugFunction -> IO [Output]
debugCollectOutputs input program f = do
    result <- debug input program f
    case result of
        Halted                        -> return []
        (WaitForOutput prog inp outp) -> fmap (outp :) (debugCollectOutputs inp prog f)

collectOutputs :: Input -> Program -> [Output]
collectOutputs input program = case execute input program of
    Halted                        -> []
    (WaitForOutput prog inp outp) -> outp : collectOutputs inp prog

part1and2 :: IO ()
part1and2 = do
    content <- readFile "resources/day9.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let buffer = replicate 1000 0
    let opArray = array (0, length numbers + length buffer  - 1) $ zip [0..] (numbers ++ buffer)
    outputs1 <- debugCollectOutputs [1] (0, 0, opArray) printInstruction
    putStrLn $ "Solution part1: " ++ unwords (fmap show outputs1)
    let outputs2 = collectOutputs [2] (0, 0, opArray)
    putStrLn $ "Solution part2: " ++ unwords (fmap show outputs2)





