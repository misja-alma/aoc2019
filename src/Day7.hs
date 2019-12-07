module Day7 where

import Data.List.Split
import Data.Array
import Data.List
import Data.Function

type Index = Int
type Program = (Index, Array Index Integer)
type Input = [Integer]
type Output = Integer
data ProgramState = Running Program Input | WaitForOutput Program Input Output | Halted   
type Operation = Input -> Program -> ProgramState
type Opcode = Integer

peek :: Array Index Integer -> Index -> Integer
peek ar pt = ar ! fromInteger (ar ! pt)

getMode :: Opcode -> Int -> Integer
getMode mode offset = case offset of
    1 -> (mode `div` 100) `mod` 10
    2 -> (mode `div` 1000) `mod` 10
    3 -> (mode `div` 10000) `mod` 10

readParam :: Array Index Integer -> Index -> Index -> Opcode -> Integer
readParam ar ip offset mode = case getMode mode offset of
    0 -> peek ar (ip + offset)
    1 -> ar ! (ip + offset)

write :: Array Index Integer -> Index -> Index -> Integer -> Array Index Integer
write ar ip offset value  = ar // [(fromInteger $ ar ! (ip + offset), value)] -- Write mode is always 0

exit :: Operation
exit _ _ = Halted

add :: Opcode -> Operation
add opcode inp (ip, ar) = 
    let p1 = readParam ar ip 1 opcode
        p2 = readParam ar ip 2 opcode
        sum = p1 + p2
        newAr = write ar ip 3 sum in
    Running (ip + 4, newAr) inp

mul :: Opcode -> Operation
mul opcode inp (ip, ar) =
    let p1 = readParam ar ip 1 opcode
        p2 = readParam ar ip 2 opcode
        product = p1 * p2
        newAr = write ar ip 3 product in
    Running (ip + 4, newAr) inp

input :: Opcode -> Operation
input _ inp (ip, ar) =
    let newAr = write ar ip 1 (head inp) in
    Running (ip + 2, newAr) (tail inp) -- TODO or maybe return WaitingForInput if no input available

output :: Opcode -> Operation
output opcode inp (ip, ar) =
   let p1 = readParam ar ip 1 opcode in
   WaitForOutput (ip + 2, ar) inp p1
   
jumpIfTrue :: Opcode -> Operation
jumpIfTrue opcode inp (ip, ar) =
      let p1 = readParam ar ip 1 opcode
          p2 = readParam ar ip 2 opcode in
      if p1 /= 0 then Running (fromInteger p2, ar) inp else Running (ip + 3, ar) inp

jumpIfFalse :: Opcode -> Operation
jumpIfFalse opcode inp (ip, ar) =
      let p1 = readParam ar ip 1 opcode
          p2 = readParam ar ip 2 opcode in
      if p1 == 0 then Running (fromInteger p2, ar) inp else Running (ip + 3, ar) inp

lessThan :: Opcode -> Operation
lessThan opcode inp (ip, ar) =
      let p1 = readParam ar ip 1 opcode
          p2 = readParam ar ip 2 opcode
          newAr = if p1 < p2 then write ar ip 3 1 else write ar ip 3 0 in
      Running (ip + 4, newAr) inp

equals :: Opcode -> Operation
equals opcode inp (ip, ar) =
      let p1 = readParam ar ip 1 opcode
          p2 = readParam ar ip 2 opcode
          newAr = if p1 == p2 then write ar ip 3 1 else write ar ip 3 0 in
      Running (ip + 4, newAr) inp

execute :: Input -> Program -> ProgramState
execute input (pc, ar) =
    let instr = ar ! pc
        operation = translate instr
        result = operation input (pc, ar) in
    case result of
        Running newProg newInput -> execute newInput newProg
        outputOrHalted           -> outputOrHalted

translate :: Opcode -> Operation
translate opcode =
    case opcode `mod` 100 of
      99 -> exit
      1  -> add opcode
      2  -> mul opcode
      3 -> input opcode
      4 -> output opcode
      5 -> jumpIfTrue opcode
      6 -> jumpIfFalse opcode
      7 -> lessThan opcode
      8 -> equals opcode

outputForPhases :: Program -> [Integer] -> Integer
outputForPhases program phases =
    let WaitForOutput _ _ a = execute [phases !! 0, 0] program
        WaitForOutput _ _ b = execute [phases !! 1, a] program
        WaitForOutput _ _ c = execute [phases !! 2, b] program
        WaitForOutput _ _ d = execute [phases !! 3, c] program
        WaitForOutput _ _ e = execute [phases !! 4, d] program in
     e

part1 :: IO ()
part1 = do
    content <- readFile "resources/day7.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let opArray = array (0, length numbers - 1) $ zip [0..] numbers
    let outputs = fmap (\c -> (c, outputForPhases (0, opArray) c)) (permutations [0,1,2,3,4])
    let bestTuple = maximumBy (compare `on` snd) outputs
    putStrLn $ "Solution: " ++ show (snd bestTuple) ++ " from " ++ concatMap show (fst bestTuple)

executeSafe :: Integer -> ProgramState -> Program -> ProgramState
executeSafe _ Halted _ = Halted
executeSafe phase (WaitForOutput _ _ outp) prog = execute [phase, outp] prog

loopUntilHalt :: Program -> Integer -> [Integer] -> Integer
loopUntilHalt program lastOutput phases =
    -- Initialise programs with phases first
    let sa = execute [phases !! 0, lastOutput] program
        sb = executeSafe (phases !! 1) sa program
        sc = executeSafe (phases !! 2) sb program
        sd = executeSafe (phases !! 3) sc program
        se = executeSafe (phases !! 4) sd program in
     case se of 
         Halted -> lastOutput
         WaitForOutput _ _ outputE ->   
             loopUntilHalt' [sa, sb, sc, sd, se] outputE

executeSafe' :: ProgramState -> ProgramState -> ProgramState
executeSafe' Halted _ = Halted
executeSafe' (WaitForOutput _ _ previousOutput) (WaitForOutput prog _ _) = execute [previousOutput] prog

getProgram :: ProgramState -> Program
getProgram (WaitForOutput prog _ _) = prog
getProgram (Running prog _) = prog
getProgram Halted = error "Cannot get program from Halted state"

loopUntilHalt' :: [ProgramState] -> Integer -> Integer
loopUntilHalt' programs lastOutput =
    -- Loop until programs stop producing output
    let sa = execute [lastOutput] (getProgram $ programs !! 0)
        sb = executeSafe' sa (programs !! 1)
        sc = executeSafe' sb (programs !! 2)
        sd = executeSafe' sc (programs !! 3)
        se = executeSafe' sd (programs !! 4) in
     case se of 
        Halted -> lastOutput
        WaitForOutput _ _ outputE ->   
            loopUntilHalt' [sa, sb, sc, sd, se] outputE   

part2 :: IO ()
part2 = do
    content <- readFile "resources/day7.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let opArray = array (0, length numbers - 1) $ zip [0..] numbers
    let outputs = fmap (\c -> (c, loopUntilHalt (0, opArray) 0 c)) (permutations [5,6,7,8,9])
    let bestTuple = maximumBy (compare `on` snd) outputs
    putStrLn $ "Solution: " ++ show (snd bestTuple) ++ " from " ++ concatMap show (fst bestTuple)
