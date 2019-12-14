module Day14 where

import Data.List.Split
import qualified Data.Map as M
import Data.Maybe

import Debug.Trace

import Utils

type Material = (String, Int)
data Reaction = Reaction [Material] Material deriving Show

outputToken :: Reaction -> String
outputToken (Reaction _ (t, _)) = t

readReaction :: String -> Reaction
readReaction s = let [is, output] = splitOn " => " s in
                 Reaction (fmap parseTuple (splitOn ", " is)) (parseTuple output)
                 where parseTuple ts = let [q,t] = splitOn " " ts in (t, read q)

mergeMaterial :: M.Map String Int -> M.Map String Int -> M.Map String Int
mergeMaterial ms1 ms2 = M.unionWith (+) ms1 ms2


calcForInput ::  M.Map String Reaction -> Int -> (M.Map String Int, M.Map String Int) -> Material -> (M.Map String Int, M.Map String Int)
calcForInput rs neededQuantity (lfs, res) (it, iq)= let required = iq * neededQuantity
                                                        (reqLeft, newLeftOvers) = case M.lookup it lfs of
                                                                                                 Just l  -> let taken = min required l in
                                                                                                            (required - taken, M.insert it (l - taken) lfs)
                                                                                                 Nothing -> (required, lfs)
                                                    in  if reqLeft > 0 then
                                                             if it == "ORE" then
                                                               (newLeftOvers, mergeMaterial res (M.singleton it reqLeft))
                                                             else
                                                               let (newerLeftOvers, neededForInput) = calculateInputs rs newLeftOvers (it, reqLeft)
                                                               in (newerLeftOvers, mergeMaterial res neededForInput)
                                                        else
                                                             (newLeftOvers, res)

-- returns a tuple of new left overs and a map of needed material
calculateInputs :: M.Map String Reaction -> M.Map String Int -> Material -> (M.Map String Int, M.Map String Int)
calculateInputs reactions leftovers (mt, mq) =
    let Reaction inputs (_, oq) = reactions M.! mt
        leftOverAmount = if oq >= mq then oq - mq else if mq `rem` oq == 0 then 0 else oq - mq `rem` oq
        neededQuantity = if oq >= mq then 1 else if mq `rem` oq == 0 then mq `div` oq else mq `div` oq + 1
        -- we need mq, reaction offers oq. Real need is then
        -- for each input: required is iq * oq
        -- take whatever is avail from the leftovers, continue with new leftovers
        -- if required > 0: recurse
        (newLeftovers, needed) = foldl (calcForInput reactions neededQuantity) (leftovers, M.empty :: M.Map String Int) (inputs :: [Material])
    in (mergeMaterial newLeftovers (M.singleton mt leftOverAmount), needed)

part1 :: IO ()
part1 = do
    content <- readFile "resources/day14.txt"
    let reactions = fmap readReaction (lines content)
    let reactionMap = M.fromList $ fmap (\r -> (outputToken r, r)) reactions
    let (Reaction _ fuel) = reactionMap M.! "FUEL"
    let (leftOvers, result) = calculateInputs reactionMap M.empty fuel
    putStrLn $ "Solution: " ++ show (result M.! "ORE")

calcFuelAndUpdateTotalOres :: M.Map String Reaction -> (M.Map String Int, Integer, Int) -> (M.Map String Int, Integer, Int)
calcFuelAndUpdateTotalOres reactions (leftOvers, total, iterations) =
    let (newLeftOvers, result) = calculateInputs reactions leftOvers ("FUEL", 1)
        ores = result M.! "ORE"
    in trace (show total) (newLeftOvers, total + fromIntegral ores, iterations + 1)

part2 :: IO ()
part2 = do
    content <- readFile "resources/day14.txt"
    let reactions = fmap readReaction (lines content)
    let reactionMap = M.fromList $ fmap (\r -> (outputToken r, r)) reactions
    let leftOvers = M.empty
    let totalOres = 0
    let (_, _, solution) = head $ dropWhile (\(_, total, _) -> total <= 1000000000000) $ iterate (calcFuelAndUpdateTotalOres reactionMap) (leftOvers, totalOres, 0)
    putStrLn $ "Solution: " ++ show (solution - 1) -- 4436981