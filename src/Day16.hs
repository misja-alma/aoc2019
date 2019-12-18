module Day16 where

import Data.Char
import Debug.Trace

basePattern = [0,1,0,-1]

generatePattern :: [Int] -> Int -> [Int]
generatePattern pattern rep =
    let repeated = basePattern >>= replicate rep
    in tail $ concat $ repeat repeated

generateNumberForLine :: [Int] -> Int -> Int
generateNumberForLine nrs lineNr =
    let pattern = generatePattern basePattern lineNr
        products = (\(a, b) -> a * b) <$> zip pattern nrs
        nrSum = sum products in
    abs $ nrSum `rem` 10

generateNextNumbers :: [Int] -> [Int]
generateNextNumbers nrs = (generateNumberForLine nrs) <$> [1..length nrs]

part1 :: IO ()
part1 = do
    content <- readFile "resources/day16.txt"
    let nrs = digitToInt <$> content
    let finalNumbers = head $ drop 100 $ iterate generateNextNumbers nrs
    let solution = take 8 finalNumbers
    putStrLn $ "Solution: " ++ concatMap show solution

part2 :: IO ()
part2 = do
    content <- readFile "resources/day16.txt"
    let nrs = concat $ replicate 130 [1,2,3,4,5] --digitToInt <$> content
    let nrs2 = take 80 $ iterate generateNextNumbers nrs
    let bla = filter (=="bla") $ fmap (\line -> trace (concatMap show $ take 80 line) "bla") nrs2
    putStrLn $ "Solution: " ++ show (length bla)