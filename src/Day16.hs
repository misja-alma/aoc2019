module Day16 where

import Data.Char
import Debug.Trace
import qualified Data.Map as M

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

calcOutput :: Int -> [Int] -> Int -> M.Map (Int, Int) Int -> Int -> (M.Map (Int, Int) Int, Int)
calcOutput nrRepeats numbers depth cache offset =
    case M.lookup (offset, depth) cache of
        Just v  -> (cache, v)
        Nothing -> calcOutput' nrRepeats numbers depth cache offset

-- TODO to avoid recursion:
-- start with outputs needed from iteration 0: Those are the ones from offset - 100 to offset + 7
-- with those cached, start iteration 1. This takes its inputs from the cached previous one, and builds a new hashmap in the process.
-- its start offset = pref offset + 1
-- iterate. Note that the first calculation is already generation 1.

calcOutput' :: Int -> [Int] -> Int -> M.Map (Int, Int) Int -> Int -> (M.Map (Int, Int) Int, Int)
calcOutput' nrRepeats numbers 0 cache offset = let repeated = concat $ replicate nrRepeats numbers
                                                   summed = sum $ drop offset repeated
                                                   result = abs $ summed `rem` 10
                                               in (M.insert (offset, 0) result cache, result)
calcOutput' nrRepeats numbers depth cache offset =
    let start = offset
        end   = (length numbers) * nrRepeats - 1
        (results, newCache) = foldl (\(rs, cache) i -> let (newCache, res) = calcOutput nrRepeats numbers (depth - 1) cache i in (res: rs, newCache)) ([], cache) [start..end] 
        result = abs $ (sum results) `rem` 10
    in (M.insert (offset, depth) result newCache, result)

part2 :: IO ()
part2 = do
    content <- readFile "resources/day16.txt"
    let nrs = digitToInt <$> content
    let offset = 5973857
    let (results, _) = foldl (\(rs, cache) i -> let (newCache, res) = calcOutput 10000 nrs 100 cache i in (res: rs, newCache)) ([], M.empty) [offset..offset+7]
    putStrLn $ "Solution: " ++ concatMap show (reverse results)

