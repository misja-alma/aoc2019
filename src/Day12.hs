module Day12 where

import Data.Function
import Text.Regex.Posix

type PositionWithVelocity = ((Int, Int, Int), (Int, Int, Int))

position :: PositionWithVelocity -> (Int, Int, Int)
position = fst

velocity :: PositionWithVelocity -> (Int, Int, Int)
velocity = snd

xComponent :: PositionWithVelocity -> (Int, Int)
xComponent ((x,_,_),(vx,_,_)) = (x,vx)

yComponent :: PositionWithVelocity -> (Int, Int)
yComponent ((_,y,_),(_,vy,_)) = (y,vy)

zComponent :: PositionWithVelocity -> (Int, Int)
zComponent ((_,_,z),(_,_,vz)) = (z,vz)

updateDimension :: Int -> Int -> Int -> Int
updateDimension v subjPos targPos
    | targPos > subjPos = v + 1
    | targPos < subjPos = v - 1
    | otherwise         = v

-- original pos, running velo, other pos
updateVelocityWith :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> PositionWithVelocity
updateVelocityWith (sx,sy,sz) (vsx,vsy,vsz) (ox,oy,oz) =
    let newVx = updateDimension vsx sx ox
        newVy = updateDimension vsy sy oy
        newVz = updateDimension vsz sz oz in
    ((sx,sy,sz), (newVx, newVy, newVz))

updatePosition :: [PositionWithVelocity] -> PositionWithVelocity -> PositionWithVelocity
updatePosition others p = let ((sx,sy,sz),(vsx,vsy,vsz)) = foldl (\newP other -> updateVelocityWith (position p) (velocity newP) (position other)) p others in
                          ((sx + vsx, sy + vsy, sz + vsz),(vsx,vsy,vsz))

updatePositions :: [PositionWithVelocity] -> [PositionWithVelocity]
updatePositions ps = fmap (\p -> updatePosition (filter (/= p) ps) p) ps

energy :: PositionWithVelocity -> Int
energy ((x,y,z),(vx,vy,vz)) = (abs x + abs y + abs z) * (abs vx + abs vy + abs vz)

readPosition :: String -> (Int, Int, Int)
readPosition s = let (_,_,_,[x,y,z]) = s =~ "<x=(-?[0-9]+), y=(-?[0-9]+), z=(-?[0-9]+)>"  :: (String,String,String,[String]) in
                 (read x, read y, read z)
part1 :: IO ()
part1 = do
    content <- readFile "resources/day12.txt"
    let positions = fmap readPosition (lines content)
    let posWithVelos = zip positions (repeat (0,0,0))
    let steps = iterate updatePositions posWithVelos
    let results = steps !! 1000
    let solution = sum $ fmap energy results
    putStrLn $ "Solution: " ++ show solution

findPeriod :: ([PositionWithVelocity] -> [PositionWithVelocity] -> Bool) -> [[PositionWithVelocity]] -> Int
findPeriod matcher ps = let matchThis = head ps in
                        1 + length (takeWhile (not . matcher matchThis) (tail ps))

eqBy :: Eq b => (a -> b) -> a -> a -> Bool
eqBy f x y = f x == f y

part2 :: IO ()
part2 = do
    content <- readFile "resources/day12.txt"
    let positions = fmap readPosition (lines content)
    let posWithVelos = zip positions (repeat (0,0,0))
    let steps = iterate updatePositions posWithVelos
    let px = findPeriod (eqBy $ fmap xComponent) steps
    let py = findPeriod (eqBy $ fmap yComponent) steps
    let pz = findPeriod (eqBy $ fmap zComponent) steps
    let gcdxy = gcd px py
    let gcdxyz = gcd gcdxy pz
    let solution = px * py * pz `div` (gcdxy * gcdxyz)
    putStrLn $ "Solution: " ++ show solution