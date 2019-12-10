module Day10 where

import Data.List
import Data.Function

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

gradient :: (Int, Int) -> (Int, Int) -> (Int, Int)
gradient (fromX, fromY) (toX, toY) = (toX - fromX, toY - fromY)

onSameLine :: (Int, Int) -> (Int, Int) -> Bool
onSameLine (dx, dy) (dx2, dy2)
    | dx == 0 || dx2 == 0 = dx2 == dx
    | dy == 0 || dy2 == 0 = dy2 == dy
    | otherwise           = let gcd1 = gcd dx dy
                                gcd2 = gcd dx2 dy2 in
                            (dx `div` gcd1 == dx2 `div` gcd2) && (dy `div` gcd1 == dy2 `div` gcd2)

sameGradientButSmaller :: (Int, Int) -> (Int, Int) -> Bool
sameGradientButSmaller g1@(dx, dy) g2@(dx2, dy2) =
  (signum dx == signum dx2) && (signum dy == signum dy2) && (dx /= dx2 || dy /= dy2) &&
      onSameLine g1 g2 && (abs dx2 + abs dy2) < (abs dx + abs dy)

-- find another cell that has same rel. gradient but smaller distance (and not the same cell)
isBlocked :: [(Int, Int)] -> (Int, Int) -> Bool
isBlocked all cell = any (sameGradientButSmaller cell) all

visibleOthers :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
visibleOthers others asteroid =
    let gradients = fmap (gradient asteroid) (filter (/= asteroid) others)
        visible = filter (not . isBlocked gradients) gradients in
    visible

rowToCells :: ([(Char, Int)], Int) -> [(Int, Int, Char)]
rowToCells (row, y) = fmap (\(c, x) -> (x,y,c)) row

part1 :: IO()
part1 = do
    content <- readFile "resources/day10.txt"
    let rowsWithX = fmap (\r -> zip r [0..]) (lines content) :: [[(Char, Int)]]
    let rowsWithXY = zip rowsWithX [0..] :: [([(Char, Int)], Int)]
    let cells = rowsWithXY >>= rowToCells
    let asteroids = (\(x,y,c) -> (x,y)) <$> filter (\(x,y,c) -> c == '#') cells
    let visible = fmap (\a -> (a, visibleOthers asteroids a)) asteroids
    let solution = maximumBy (compare `on` (\(_, vs) -> length vs)) visible
    putStrLn $ "Solution: " ++ show (length (snd solution))

-- Note that dy had to be reversed for this problem: (0,-1) has angle 0.
angle :: (Int, Int) -> Double
angle (dx, dy) = if dy == 0 then (if dx < 0 then -1.5 * pi else pi / 2) else
                   let result = atan (fromIntegral dx / fromIntegral (-dy)) in
                   if dy > 0 then pi + result
                             else if dx < 0 then 2 * pi + result
                                  else result

uniqueAngle :: [(Int, Int)] -> (Int, Int) -> Double
uniqueAngle gs vect =
    let nrBlocking = count (sameGradientButSmaller vect) gs in
    angle vect + fromIntegral nrBlocking * 2 * pi

compareByAngle :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Ordering
compareByAngle withGradients = compare `on` uniqueAngle withGradients

part2 :: IO()
part2 = do
    content <- readFile "resources/day10.txt"
    let rowsWithX = fmap (\r -> zip r [0..]) (lines content) :: [[(Char, Int)]]
    let rowsWithXY = zip rowsWithX [0..] :: [([(Char, Int)], Int)]
    let cells = rowsWithXY >>= rowToCells
    let asteroids = (\(x,y,c) -> (x,y)) <$> filter (\(x,y,c) -> c == '#') cells
    let origin = (22,25)
    let (ox, oy) = origin
    let gradients = gradient origin <$> filter (/= origin) asteroids
    let sorted = sortBy (compareByAngle gradients) gradients
    let (dx,dy) = sorted !! 200
    let solution = (ox + dx) * 100 + (oy + dy)
    putStrLn $ "Solution : " ++ show solution