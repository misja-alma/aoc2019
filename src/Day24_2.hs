module Day24_2 where

import qualified Data.Set as S
import Data.Array
import Utils
import Debug.Trace

rowToCells :: (String, Int) -> [(Int, Int, Char)]
rowToCells (cs, y) = (\(c, x) -> (x,y,c)) <$> zip cs [0..]

type Grid = Array (Int, Int, Int) Char
gridWidth =  5
gridHeight = 5
gridDepth = 300

bugAt :: Grid -> (Int, Int, Int) -> Bool
bugAt grid xyz = grid ! xyz == '#'

isValid :: (Int, Int, Int) -> Bool
isValid (x,y,z) = x >= 0 && y >= 0 && z >=0 && x < gridWidth && y < gridHeight && z < gridDepth && not (x == 2 && y == 2)

countSpecialNeighbours :: Int -> Int -> Int -> Grid -> Int
countSpecialNeighbours x y z grid
  -- middle: neighbours from lower
  | x == 3 && y == 2 = if z < gridDepth -1 then count (bugAt grid) [(4,0,z+1),(4,1,z+1),(4,2,z+1),(4,3,z+1),(4,4,z+1)] else 0 -- right edge
  | x == 1 && y == 2 = if z < gridDepth -1 then count (bugAt grid) [(0,0,z+1),(0,1,z+1),(0,2,z+1),(0,3,z+1),(0,4,z+1)] else 0 -- left edge
  | x == 2 && y == 3 = if z < gridDepth -1 then count (bugAt grid) [(0,4,z+1),(1,4,z+1),(2,4,z+1),(3,4,z+1),(4,4,z+1)] else 0 -- bottom edge
  | x == 2 && y == 1 = if z < gridDepth -1 then count (bugAt grid) [(0,0,z+1),(1,0,z+1),(2,0,z+1),(3,0,z+1),(4,0,z+1)] else 0 -- top edge
  -- edges: neighbours from above
  | x == 0 && y /= 0 && y /= 4 = if z > 0 then count (bugAt grid) [(1,2,z-1)] else 0
  | x == 4 && y /= 0 && y /= 4 = if z > 0 then count (bugAt grid) [(3,2,z-1)] else 0
  | y == 0 && x /= 0 && x /= 4 = if z > 0 then count (bugAt grid) [(2,1,z-1)] else 0
  | y == 4 && x /= 0 && x /= 4 = if z > 0 then count (bugAt grid) [(2,3,z-1)] else 0
  -- corners
  | x == 0 && y == 0 =  if z > 0 then count (bugAt grid) [(1,2,z-1),(2,1,z-1)] else 0
  | x == 0 && y == 4 =  if z > 0 then count (bugAt grid) [(1,2,z-1),(2,3,z-1)] else 0
  | x == 4 && y == 0 =  if z > 0 then count (bugAt grid) [(2,1,z-1),(3,2,z-1)] else 0
  | x == 4 && y == 4 =  if z > 0 then count (bugAt grid) [(3,2,z-1),(2,3,z-1)] else 0
  | otherwise = 0


calcNewPoint :: Int -> Int -> Int -> Grid -> Char
calcNewPoint x y z grid = let normalNeighbours = count (\p -> isValid p && bugAt grid p) [(x-1, y, z), (x, y-1, z), (x+1, y, z), (x, y+1, z)]
                              specialNeighbours = countSpecialNeighbours x y z grid
                          in case normalNeighbours + specialNeighbours of
                                 1         -> '#'
                                 2         -> if bugAt grid (x,y,z) then '.' else '#'
                                 _         -> '.'

countBugs :: Grid -> Int
countBugs grid = count (== '#') (elems grid)

calcNextGrid :: Grid -> Grid
calcNextGrid grid =
    let newPoints = [((x,y,z), if isValid (x,y,z) then calcNewPoint x y z grid else '.') | x <- [0..gridWidth-1], y <- [0..gridWidth-1], z <- [0..gridDepth-1]]
    in array ((0,0,0), (gridWidth - 1,gridHeight - 1,gridDepth - 1)) newPoints

part2 :: IO ()
part2 = do
    content <- readFile "resources/day24.txt"
    let cells = zip (lines content) [0..] >>= rowToCells
    let initialGrid = listArray ((0,0,0), (gridWidth - 1,gridHeight - 1,gridDepth - 1)) (replicate (gridWidth * gridHeight * gridDepth) '.')
    let grid = foldl (\g (x,y,v) -> g // [((x,y,150), v)]) initialGrid cells -- initial cells are in the middle
    let finalGrid = iterate calcNextGrid grid !! 200
    putStrLn $ "Solution: " ++ show (countBugs finalGrid)


showLine :: Grid -> Int -> Int ->  String
showLine grid z y = fmap (\x -> grid ! (x, y, z)) [0 .. gridWidth-1]

display :: Grid -> Int -> String
display grid z = unlines $ fmap (showLine grid z) [0 .. gridHeight-1]