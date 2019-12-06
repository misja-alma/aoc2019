module Day6 where

import Data.List
import Data.List.Split
import Text.Printf (printf)
import Data.Maybe
import Tree

mapToDepth :: Int -> Tree a -> Tree Int
mapToDepth depth (Tree v children) = Tree depth (fmap (mapToDepth (depth + 1)) children)

deepestNodeWith :: Tree String -> (Tree String -> Bool) -> String
deepestNodeWith (Tree v children) f = case find f children of
                                        Nothing -> v
                                        Just t  -> deepestNodeWith t f

part1 :: IO ()
part1 = do
      content <- readFile "resources/day6.txt"
      let pairs = splitOn ")" <$> lines content
      let tree = mkTree "COM" pairs
      let depths = mapToDepth 0 tree
      printf  "Solution :: %d\n" $ treeSum depths

part2 :: IO ()
part2 = do
      content <- readFile "resources/day6.txt"
      let pairs = splitOn ")" <$> lines content
      let tree = mkTree "COM" pairs
      let you = depth "YOU" tree
      let san = depth "SAN" tree
      let mutualParent = deepestNodeWith tree (\n -> hasChild "YOU" n && hasChild "SAN" n)
      -- subtract 2 from the result because we need the shortest path between the parents, except if both nodes are on the same branch
      let solution = if mutualParent == "YOU" || mutualParent == "SAN" then abs (you - san) else you + san - 2 * depth mutualParent tree - 2
      printf  "Solution :: %d\n" solution