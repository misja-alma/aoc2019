module Day5 where

import Data.List.Split
import Data.Array
import Data.List

type Index = Int
type Program = Array Index Integer
type Input = Integer
type Operation = Input -> Program -> Index -> IO (Maybe (Program, Index))
type Opcode = Integer

peek :: Program -> Index -> Integer
peek ar pt = ar ! fromInteger (ar ! pt)

getMode :: Opcode -> Int -> Integer
getMode mode offset = case offset of
    1 -> (mode `div` 100) `mod` 10
    2 -> (mode `div` 1000) `mod` 10
    3 -> (mode `div` 10000) `mod` 10

readParam :: Program -> Index -> Index -> Opcode -> Integer
readParam ar ip offset mode = case getMode mode offset of
    0 -> peek ar (ip + offset)
    1 -> ar ! (ip + offset)

write :: Program -> Index -> Index -> Integer -> Program
write ar ip offset value  = ar // [(fromInteger $ ar ! (ip + offset), value)] -- Write mode is always 0

exit :: Operation
exit _ _ _ = return Nothing

add :: Opcode -> Operation
add opcode _ ar ip = do
    let p1 = readParam ar ip 1 opcode
    let p2 = readParam ar ip 2 opcode
    let sum = p1 + p2
    let newAr = write ar ip 3 sum
    return $ Just (newAr, ip + 4)

mul :: Opcode -> Operation
mul opcode _ ar ip = do
    let p1 = readParam ar ip 1 opcode
    let p2 = readParam ar ip 2 opcode
    let product = p1 * p2
    let newAr = write ar ip 3 product
    return $ Just (newAr, ip + 4)

input :: Opcode -> Operation
input _ inp ar ip = do
    let newAr = write ar ip 1 inp
    return $ Just (newAr, ip + 2)

output :: Opcode -> Operation
output opcode _ ar ip = do
   let p1 = readParam ar ip 1 opcode
   print p1
   return $ Just (ar, ip + 2)

jumpIfTrue :: Opcode -> Operation
jumpIfTrue opcode _ ar ip = do
      let p1 = readParam ar ip 1 opcode
      let p2 = readParam ar ip 2 opcode
      if p1 /= 0 then return $ Just (ar, fromInteger p2) else return $ Just (ar, ip + 3)

jumpIfFalse :: Opcode -> Operation
jumpIfFalse opcode _ ar ip = do
      let p1 = readParam ar ip 1 opcode
      let p2 = readParam ar ip 2 opcode
      if p1 == 0 then return $ Just (ar, fromInteger p2) else return $ Just (ar, ip + 3)

lessThan :: Opcode -> Operation
lessThan opcode _ ar ip = do
      let p1 = readParam ar ip 1 opcode
      let p2 = readParam ar ip 2 opcode
      let newAr = if p1 < p2 then write ar ip 3 1 else write ar ip 3 0
      return $ Just (newAr, ip + 4)

equals :: Opcode -> Operation
equals opcode _ ar ip = do
      let p1 = readParam ar ip 1 opcode
      let p2 = readParam ar ip 2 opcode
      let newAr = if p1 == p2 then write ar ip 3 1 else write ar ip 3 0
      return $ Just (newAr, ip + 4)

execute :: Input -> Program -> Index -> IO Integer
execute input ar pc = do
    let instr = ar ! pc
    let operation = translate instr
    result <- operation input ar pc
    case result of
        Just (newAr, newPc) -> execute input newAr newPc
        Nothing             -> return (ar ! 0)

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

part1 :: IO ()
part1 = do
    content <- readFile "resources/day5.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let opArray = array (0, length numbers - 1) $ zip [0..] numbers
    execute 1 opArray 0
    putStrLn "Finished!"

part2 :: IO ()
part2 = do
    content <- readFile "resources/day5.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let opArray = array (0, length numbers - 1) $ zip [0..] numbers
    execute 5 opArray 0
    putStrLn "Finished!"
