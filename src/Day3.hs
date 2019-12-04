module Day3 where

import Data.List.Split
import Data.List
import Utils

data Point = Point Int Int deriving (Show, Eq)

data Line = Line Point Point deriving Show

toLine :: [Point] -> Line
toLine xs = Line (xs !! 0) (xs !! 1)

origin = Point 0 0

sliding :: Int -> Int -> [a] -> [[a]]
sliding width step xs
    | len < width = []
    | otherwise   = [(take width xs)] ++ sliding width step (drop step xs)
         where
            len = length xs

crossingPoints :: [Line] -> Line -> [Point]
crossingPoints lines line = lines >>= (crossingPoint line)

deltas :: Line -> (Int, Int)
deltas (Line (Point x1 y1) (Point x2 y2)) = (x2 - x1, y2 - y1)

-- Assumes that overlapping lines don't count
crossingPoint :: Line -> Line -> [Point]
crossingPoint v1 v2 = let (dx1, dy1) = deltas v1
                          (dx2, dy2) = deltas v2
                          Line (Point x11 y11) (Point x12 y12) = v1
                          Line (Point x21 y21) (Point x22 y22) = v2 in
                      if dx1 == 0 then
                        if dx2 == 0 then [] else if within (y11, y12) y21 && within (x21, x22) x11 then [Point x11 y21] else []
                      else
                        if dy2 == 0 then [] else if within (x11, x12) x21 && within (y21, y22) y11 then [Point x21 y11] else []

nextPoint :: Point -> String -> Point
nextPoint (Point x y) direction = let orientation = head direction
                                      distance = read $ tail direction in
                           case orientation of
                             'R' -> Point (x + distance) y
                             'L' -> Point (x - distance) y
                             'U' -> Point x (y - distance)
                             'D' -> Point x (y + distance)

part1 :: IO ()
part1 = do
    content <- readFile "resources/day3.txt"
    let [ln1, ln2] = lines content
    let directions1 = splitOn "," ln1
    let directions2 = splitOn "," ln2
    let lines1 = fmap toLine $ sliding 2 1 $ scanl nextPoint origin directions1
    let lines2 = fmap toLine $ sliding 2 1 $ scanl nextPoint origin directions2
    let crossings = filter (/= origin) $ lines1 >>= (crossingPoints lines2)
    let solution = minimum $ fmap (\(Point x  y) -> (abs x) + (abs y)) crossings
    putStrLn $ "Solution: " ++ show solution

pointIsOn :: Line -> Point -> Bool
pointIsOn line (Point x y) = let Line (Point x1 y1) (Point x2 y2) = line in within (x1, x2) x && within (y1, y2) y

lineLength :: Line -> Int
lineLength line = let (dx, dy) = deltas line in (abs dx) + (abs dy)

stepsToCrossing :: Point -> [Line] -> Int
stepsToCrossing c (l : ls) = if pointIsOn l c then
                               let Line pt1 p2 = l in
                               lineLength (Line pt1 c)
                             else
                                (lineLength l) + stepsToCrossing c ls

part2 :: IO ()
part2 = do
    content <- readFile "resources/day3.txt"
    let [ln1, ln2] = lines content
    let directions1 = splitOn "," ln1
    let directions2 = splitOn "," ln2
    let lines1 = fmap toLine $ sliding 2 1 $ scanl nextPoint origin directions1
    let lines2 = fmap toLine $ sliding 2 1 $ scanl nextPoint origin directions2
    let crossings = filter (/= origin) $ lines1 >>= (crossingPoints lines2)
    let solution = minimum $ fmap (\c -> (stepsToCrossing c lines1) + (stepsToCrossing c lines2)) crossings
    putStrLn $ "Solution: " ++ show solution