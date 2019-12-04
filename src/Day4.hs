module Day4 where

import Data.List
import Utils

twoSameDigits :: Int -> Bool
twoSameDigits x = length (nub $ show x) < 6

exactlyTwoSameDigits :: Int -> Bool
exactlyTwoSameDigits x = search2Combi (show x) []

search2Combi :: String -> String -> Bool
search2Combi [] currentCombi = length currentCombi == 2
search2Combi (c: cs) []      = search2Combi cs [c]
search2Combi (c: cs) currentCombi
  | c == head currentCombi   = search2Combi cs (c: currentCombi)
  | length currentCombi == 2 = True
  | otherwise                = search2Combi cs [c]

increasingNumbers :: [Int]
increasingNumbers = do
  x1 <- [0..9]
  x2 <- [x1..9]
  x3 <- [x2..9]
  x4 <- [x3..9]
  x5 <- [x4..9]
  x6 <- [x5..9]
  return $ toNumber [x1, x2, x3, x4, x5, x6]

part1 :: IO()
part1 = do
  let inRange = filter (within (153517, 630395)) increasingNumbers
  let solution = length $ filter twoSameDigits inRange
  putStrLn $ "Solution: " ++ show solution

part2 :: IO()
part2 = do
   let inRange = filter (within (153517, 630395)) increasingNumbers
   let solution = length $ filter exactlyTwoSameDigits inRange
   putStrLn $ "Solution: " ++ show solution