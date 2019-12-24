module Day24 where

import qualified Data.Set as S
import Data.Array
import Utils

rowToCells :: (String, Int) -> [(Int, Int, Char)]
rowToCells (cs, y) = (\(c, x) -> (x,y,c)) <$> zip cs [0..]

type Grid = Array (Int, Int) Char
gridWidth =  5
gridHeight = 5

bugAt :: Grid -> (Int, Int) -> Bool
bugAt grid xy = grid ! xy == '#'

calcSum :: Grid -> Int
calcSum grid = sum [if bugAt grid (x, y) then 2 ^ (y * gridHeight + x) else 0 | x <- [0..gridWidth-1], y <- [0..gridWidth-1]]

isValid :: (Int, Int) -> Bool
isValid (x,y) = x >= 0 && y >= 0 && x < gridWidth && y < gridHeight

calcNewPoint :: Int -> Int -> Grid -> Char
calcNewPoint x y grid = let neighBours = count (\p -> isValid p && bugAt grid p) [(x-1, y), (x, y-1), (x+1, y), (x, y+1)]
                        in case neighBours of
                               1         -> '#'
                               2         -> if bugAt grid (x,y) then '.' else '#'
                               _         -> '.'

calcNextGrid :: (Grid, Int, S.Set Int) -> (Grid, Int, S.Set Int)
calcNextGrid (grid, lastSum, previousSums) =
    let newPoints = [((x,y), calcNewPoint x y grid) | x <- [0..gridWidth-1], y <- [0..gridWidth-1]]
        newGrid = array ((0,0), (gridWidth - 1,gridHeight - 1)) newPoints
    in (newGrid, calcSum newGrid, S.insert lastSum previousSums)

part1 :: IO ()
part1 = do
    content <- readFile "resources/day24.txt"
    let cells = zip (lines content) [0..] >>= rowToCells
    let initialGrid = listArray ((0,0), (gridWidth - 1,gridHeight - 1)) (replicate (gridWidth * gridHeight) '.')
    let grid = foldl (\g (x,y,v) -> g // [((x,y), v)]) initialGrid cells
    let initialState = (grid, calcSum grid, S.empty)
    let (_, solution, _) = head $ dropWhile (\(_, lastSum, previousSums) -> not $ S.member lastSum previousSums) $ iterate calcNextGrid initialState
    putStrLn $ "Solution: " ++ show solution
