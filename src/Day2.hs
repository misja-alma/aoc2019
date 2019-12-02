module Day2 where

import Data.List.Split
import Data.Array
import Data.List

type Index = Int
type Program = Array Index Integer

peek :: Program -> Index -> Integer
peek ar pt = ar ! fromInteger (ar ! pt)

add :: Program -> Index -> Index -> Index -> Program
add ar p1 p2 output = let sum = (peek ar p1) + (peek ar p2) in
                      ar // [(fromInteger $ ar ! output, sum)]

mul :: Program -> Index -> Index -> Index -> Program
mul ar p1 p2 output = let product = (peek ar p1) * (peek ar p2) in
                      ar // [(fromInteger $ ar ! output, product)]
                      
-- TODO better to first translate the instruction, then have instruction generate Either[Finished, Program], repeat until finished.
-- This way it is easier to detect deadloops and to extend with more instructions.                                            
execute :: Program -> Index -> Integer
execute ar pc = case (ar ! pc) of
    99 -> ar ! 0
    1  -> let newAr = add ar (pc + 1) (pc + 2) (pc + 3) in execute newAr (pc + 4)
    2  -> let newAr = mul ar (pc + 1) (pc + 2) (pc + 3) in execute newAr (pc + 4)

runProgramWith :: Program -> Integer -> Integer -> Integer
runProgramWith ar noun verb =
  let updated = ar // [(1, noun), (2, verb)] in
  execute updated 0

part1 :: IO ()
part1 = do
    content <- readFile "resources/day2.txt"
    let numbers = fmap (read :: [Char] -> Integer) $ splitOn "," content
    let opArray = array (0, length numbers - 1) $ zip [0..] numbers
    putStrLn $ "Solution: " ++ (show $ runProgramWith opArray 12 2)

part2 :: IO ()
part2 = do
    content <- readFile "resources/day2.txt"
    let numbers = fmap (read :: [Char] -> Integer) $ splitOn "," content
    let opArray = array (0, length numbers - 1) $ zip [0..] numbers
    -- try all nouns and verbs until program exits with value 19690720
    let xy = [(x, y)| x <- [0..100], y <- [0..100]]
    let Just (noun, verb) = find (\(n, v) -> runProgramWith opArray n v == 19690720) xy
    putStrLn $ "Solution: " ++ show (noun * 100 + verb)
