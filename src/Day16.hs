module Day16 where

import Data.Char
import Debug.Trace
import qualified Data.Map.Strict as M
import Data.Maybe

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
    let nrs = digitToInt <$> content
    let offset = 5973857
    let nrRepeats = 10000
    let end = (length nrs) * nrRepeats - 1
    let initialCache = M.fromList (zip [offset..] (drop offset (concat $ replicate nrRepeats nrs)))
    let results0 = fmap (initialCache M.!) [offset..offset+7]
    putStrLn $ "Initial ending: " ++ printResults initialCache (end - 7) end
    let cacheAfter100 = head $ drop 100 $ iterate (\cache -> calcOutputsForGeneration cache offset end) initialCache
    putStrLn $ "Solution: " ++ printResults cacheAfter100 offset (offset + 7)

printResults :: M.Map Int Int -> Int -> Int -> String
printResults cache offsetFrom end = concatMap show $ fmap (cache M.!) [offsetFrom..end]
-- gen 0: elems[0,x] = inputs[x]
-- gen 1: elems[1,x] = f (elems[0,x], elems[0,x+1] ..)
-- gen 2: elems[2,x] = f (elems[1,x], elems[1,x+1] ..)

calcOutputsForGeneration :: M.Map Int Int -> Int -> Int -> M.Map Int Int
calcOutputsForGeneration cache offsetFrom end =
    let  partialSums = tail $ scanl (\s i -> s + (cache M.! i)) 0 [end, (end-1)..(offsetFrom)]
         outputs = fmap (\(i, v)-> (i, absMod v)) (zip [end, (end-1)..offsetFrom] partialSums)
         result = M.fromList outputs
    in result

absMod :: Int -> Int
absMod x = x `rem` 10 -- No need for abs anymore