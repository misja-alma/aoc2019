module Day8 where

import Data.Function
import Utils
import Data.List
import Data.Char

width = 25
height = 6

part1 :: IO()
part1 = do
    content <- readFile "resources/day8.txt"
    let numbers = fmap digitToInt content
    let layers = sliding (width * height) (width * height) numbers
    let least0s = minimumBy (compare `on` count (== 0)) layers
    let solution = (count (== 1) least0s) * (count (== 2) least0s)
    putStrLn $ "Solution: " ++ show solution

mapPixel :: [Int] -> Int
mapPixel [] = 0
mapPixel (p: ps)
    | p == 0    = 1 -- black
    | p == 1    = 0 -- white
    | otherwise = mapPixel ps -- transparent

mapLayers :: [[Int]] -> [Int]
mapLayers ls = if null (head ls) then [] else mapPixel (fmap head ls) : mapLayers (fmap tail ls)

pixelToChar :: Int -> Char
pixelToChar p = if p == 1 then ' ' else '*'

display :: Int -> Int -> [Int] -> String
display w h ps = unlines $ fmap (fmap pixelToChar) (sliding w w ps)

part2 :: IO()
part2 = do
    content <- readFile "resources/day8.txt"
    let numbers = fmap digitToInt content
    let layers = sliding (width * height) (width * height) numbers
    let mapped = mapLayers layers
    putStrLn $ display width height mapped