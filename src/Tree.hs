module Tree (Tree (..), dfs, depth, hasChild, mkTree, treeSum, treeMap, treeFold) where

import Data.List
import Data.Maybe
import qualified Data.MultiMap as MMap

data Tree a = Tree a [Tree a]
  deriving (Eq)

showWithDepth :: (Show a) => Int -> Tree a -> String
showWithDepth depth (Tree e cs) = replicate depth ' ' ++ show e ++ "\n" ++ concatMap (showWithDepth (depth+1)) cs

instance (Show a) => Show (Tree a) where
    show = showWithDepth 0

-- Returns the first path to a (excluding a itself) that is found using depth first search.
dfs :: Eq a => a -> Tree a -> Maybe [a]
dfs x (Tree v children) = if x == v then Just [] else (v :) <$> listToMaybe (mapMaybe (dfs x) children)

depth :: Eq a => a -> Tree a -> Int
depth s tree = case dfs s tree of
                 Just path -> length path
                 Nothing   -> error "No path found to calculate depth."

hasChild :: Eq a => a -> Tree a -> Bool
hasChild s (Tree v children) = s == v || or (fmap (hasChild s) children)

treeSum :: Num a => Tree a -> a
treeSum (Tree v children) = v + sum (fmap treeSum children)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Tree v children) = Tree (f v) (fmap (treeMap f) children)

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f acc (Tree v children) = foldl (treeFold f) (f acc v) children

mkTree :: Ord a => a -> [[a]] -> Tree a
mkTree root pairs = let adjPairs = foldl (\map [from, to] -> MMap.insert from to map) MMap.empty pairs in
                    mapToTree adjPairs root

mapToTree :: Ord a => MMap.MultiMap a a -> a -> Tree a
mapToTree pairs rootValue =
    let root = Tree rootValue []
        children = MMap.lookup rootValue pairs
        childTrees = fmap (mapToTree pairs) children in
    Tree rootValue childTrees