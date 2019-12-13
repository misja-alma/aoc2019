{-# LANGUAGE NamedFieldPuns #-}

module Day13 where

import Data.List.Split
import Data.Array
import Data.List
import Data.Function
import Utils
import Data.Maybe

import Debug.Trace

type Index = Int
type Base = Index
type Program = (Index, Base, Array Index Integer)
type Input = [Integer]
type Output = Integer
data ProgramState = Running Program Input | WaitForOutput Program Input Output | Halted
type Operation = Input -> Program -> ProgramState
type Opcode = Integer

debug = flip trace

peek :: Array Index Integer -> Index -> Base -> Integer
peek ar pt base = ar ! (fromInteger (ar ! pt) + base)

getMode :: Opcode -> Int -> Integer
getMode mode offset = case offset of
    1 -> (mode `div` 100) `mod` 10
    2 -> (mode `div` 1000) `mod` 10
    3 -> (mode `div` 10000) `mod` 10

readParam :: Program -> Index -> Opcode -> Integer
readParam (ip, base, ar) offset mode = case getMode mode offset of
    0 -> peek ar (ip + offset) 0
    1 -> ar ! (ip + offset)
    2 -> peek ar (ip + offset) base

write :: Program -> Index -> Opcode -> Integer -> Array Index Integer
write (ip, base, ar) offset mode value = case getMode mode offset of
    2 -> ar // [(base + fromInteger (ar ! (ip + offset)), value)]
    _ -> ar // [(fromInteger $ ar ! (ip + offset), value)]

exit :: Operation
exit _ _ = Halted

add :: Opcode -> Operation
add opcode inp prog@(ip, base, ar) =
    let p1 = readParam prog 1 opcode
        p2 = readParam prog 2 opcode
        sum = p1 + p2
        newAr = write prog 3 opcode sum in
    Running (ip + 4, base, newAr) inp

mul :: Opcode -> Operation
mul opcode inp prog@(ip, base, ar) =
    let p1 = readParam prog 1 opcode
        p2 = readParam prog 2 opcode
        product = p1 * p2
        newAr = write prog 3 opcode product in
    Running (ip + 4, base, newAr) inp

input :: Opcode -> Operation
input opcode inp prog@(ip, base, ar) =
    let newAr = write prog 1 opcode (head inp) in
    Running (ip + 2, base, newAr) (tail inp) -- TODO or maybe return WaitingForInput if no input available

output :: Opcode -> Operation
output opcode inp prog@(ip, base, ar) =
   let p1 = readParam prog 1 opcode in
   WaitForOutput (ip + 2, base, ar) inp p1

jumpIfTrue :: Opcode -> Operation
jumpIfTrue opcode inp prog@(ip, base, ar) =
      let p1 = readParam prog 1 opcode
          p2 = readParam prog 2 opcode in
      if p1 /= 0 then Running (fromInteger p2, base, ar) inp else Running (ip + 3, base, ar) inp

jumpIfFalse :: Opcode -> Operation
jumpIfFalse opcode inp prog@(ip, base, ar) =
      let p1 = readParam prog 1 opcode
          p2 = readParam prog 2 opcode in
      if p1 == 0 then Running (fromInteger p2, base, ar) inp else Running (ip + 3, base, ar) inp

lessThan :: Opcode -> Operation
lessThan opcode inp prog@(ip, base, ar) =
      let p1 = readParam prog 1 opcode
          p2 = readParam prog 2 opcode
          newAr = if p1 < p2 then write prog 3 opcode 1 else write prog 3 opcode 0 in
      Running (ip + 4, base, newAr) inp

equals :: Opcode -> Operation
equals opcode inp prog@(ip, base, ar) =
      let p1 = readParam prog 1 opcode
          p2 = readParam prog 2 opcode
          newAr = if p1 == p2 then write prog 3 opcode 1 else write prog 3 opcode 0 in
      Running (ip + 4, base, newAr) inp

setBase :: Opcode -> Operation
setBase opcode inp prog@(ip, base, ar) =
    let p1 = readParam prog 1 opcode in
    Running (ip + 2, base + fromInteger p1, ar) inp

execute :: Input -> Program -> ProgramState
execute input prog@(pc, base, ar) =
    let instr = ar ! pc
        operation = translate instr
        result = operation input prog in
    case result of
        Running newProg newInput -> execute newInput newProg
        outputOrHalted           -> outputOrHalted

translate :: Opcode -> Operation
translate opcode =
    case opcode `mod` 100 of
      99 -> exit
      1  -> add opcode
      2  -> mul opcode
      3  -> input opcode
      4  -> output opcode
      5  -> jumpIfTrue opcode
      6  -> jumpIfFalse opcode
      7  -> lessThan opcode
      8  -> equals opcode
      9  -> setBase opcode

type Grid = Array (Index, Index) Int

collectOutputs :: Input -> Program -> [Output]
collectOutputs input program = case execute input program of
    Halted                        -> []
    (WaitForOutput prog inp outp) -> outp : collectOutputs inp prog

part1 :: IO ()
part1 = do
    content <- readFile "resources/day13.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let buffer = replicate 1000 0
    let opArray = array (0, length numbers + length buffer  - 1) $ zip [0..] (numbers ++ buffer)
    let outputs = collectOutputs [] (0,0,opArray)
    let solution = count (\[_,_,id] -> id == 2) $ sliding 3 3 outputs
    putStrLn $ "Solution: " ++ show solution -- 268

drawItem :: Grid -> [Int] -> Grid
drawItem grid [x,y,it] = grid // [((x,y), it)]

showPixel :: Int -> Char
showPixel 0  = ' '
showPixel 1  = '#'
showPixel 2  = 'X'
showPixel 3  = '_'
showPixel 4  = 'o'

showLine :: Grid -> Int -> Int -> Int -> String
showLine grid startX endX y = fmap (\x -> showPixel $ grid ! (x, y)) [startX .. endX]

display :: Grid -> String
display grid = let startY = 0
                   endY = gridHeight - 1
                   startX = 0
                   endX = gridWidth - 1 in
               unlines $ fmap (showLine grid startX endX) [startY .. endY]

gridWidth = 45
gridHeight = 35

findPos :: Int -> Grid -> (Int, Int)
findPos o grid = let coords = [(x,y)| x<-[0..gridWidth-1], y<-[0..gridHeight-1]] in
                 fromJust $ find (\xy -> grid ! xy == o) coords

collect3Outputs :: Input -> Program -> (Program, [Int])
collect3Outputs input program = let WaitForOutput prog1 inp outp1 = execute input program
                                    WaitForOutput prog2 _ outp2 = execute [] prog1
                                    WaitForOutput prog3 _ outp3 = execute [] prog2 in
                                trace ("output1: " ++ show outp1 ++ " output2: " ++ show outp2 ++ " output3: " ++ show outp3) (prog3, [fromInteger outp1, fromInteger outp2, fromInteger outp3])

gameFinished :: [Int] -> Bool
gameFinished [x,y,_] = x == -1 && y == 0

collectOutputsAndScore :: Input -> Program -> (Program, Int, [[Int]])
collectOutputsAndScore input program =
    let outputs = tail $ iterate (\(pr, os) -> collect3Outputs input pr) (program, [])
        os = takeWhile (not . gameFinished) (fmap snd outputs)
        (finalProg, [_,_,score]) = head $ dropWhile (not . gameFinished . snd) outputs in
    (finalProg, score, reverse $ tail os)

part2 :: IO ()
part2 = do
    content <- readFile "resources/day13.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let buffer = replicate 1000 0

    let opArray = array (0, length numbers + length buffer  - 1) $ zip [0..] (numbers ++ buffer)
     -- Set mem address 0 to 2
    let gameProg = (0,0,opArray // [(0, 2)])
    let (prog2, score, outputs) = collectOutputsAndScore [] gameProg

    let initialGrid = listArray ((0,0), (gridWidth - 1,gridHeight - 1)) (replicate (gridWidth * gridHeight) (0))
    let grid = foldl drawItem initialGrid outputs
    putStrLn $ display grid

    let ballPos = findPos 4 grid
    let paddlePos = findPos 3 grid
    putStrLn $ "Ball: " ++ show ballPos ++ " paddle: " ++ show paddlePos

    let initialState = Playing prog2 ballPos paddlePos score
    let states = dropWhile isPlaying $ iterate updatePaddle initialState
    let Finished finalScore = head states
    print finalScore -- 13989


-- first: either game finished or paddle moved if applicable
-- if brick crushed: second is score. There might be multiple bricks crushed. We can continue reading as normal afterwards
-- Finally there is always a new ball position.
-- TODO
--      the reading can actually be done in pairs, and the 2nd el can be checked and processed depending on its type
--      2nd el can be paddle, ball or a score after a crush, in that case it is recognized by its x
--      this way we don't need the conditional check for reading the paddle pos
--      reading can stop as soon as a ball is read
-- TODO it seems the game finished comes as 2nd line for the ball!

data GameState = Playing Program (Int, Int) (Int, Int) Int | Finished Int

isPlaying :: GameState -> Bool
isPlaying (Finished _) = False
isPlaying _            = True

--updatePaddle' :: GameState -> GameState
--updatePaddle' state1@(Playing prog (ballX, ballY) (pX, pY) s) =
--  let joystick = trace "updatePaddle" $ if ballX > pX then 1 else if ballX < pX then -1 else 0
--      -- TODO iterate over couples of 3outputs. Turn them into actions (and execute them immediately). Dropwhile action is not Ball or Finished.
--      crushes = iterate crushBrick state1
--                        where crushBrick (Playing newProg2, newS, (oldX2, oldY2)) =
--                                  let (prog5, [_, _, newScore]) = collect3Outputs [] newProg2
--                                      (prog6, [newerOldX, newerOldY, _]) = collect3Outputs [] prog5
--                                  in (prog6, newScore, (newerOldX, newerOldY))
--
--                (newerProg2, newerS, _) = head $ dropWhile (\(_, _, (ox, oy)) -> ox /= -1 || oy /= ballY) crushes

updatePaddle :: GameState -> GameState
updatePaddle (Playing prog (ballX, ballY) (pX, pY) s) =
  let joystick = trace "updatePaddle" $ if ballX > pX then 1 else if ballX < pX then -1 else 0
      (prog1, os@[oldX, oldY, maybeFinalScore]) = collect3Outputs [joystick] prog in
  if gameFinished os then Finished maybeFinalScore
  else
      -- optionally read paddle pos when we know it moved
      let (newProg, newPaddle, (newOldX, newOldY)) =
            if joystick /= 0 then
                let (prog2, [pX2, pY2, _]) = collect3Outputs [] prog1
                    (prog3, [x3, y3, _]) = collect3Outputs [] prog2 in
                (prog3, (pX2, pY2), (x3, y3))
            else (prog1, (pX, pY), (oldX, oldY))

          crushes = iterate crushBrick (newProg, s, (newOldX, newOldY))
                        where crushBrick (newProg2, newS, (oldX2, oldY2)) =
                                  let (prog5, [_, _, newScore]) = collect3Outputs [] newProg2
                                      (prog6, [newerOldX, newerOldY, _]) = collect3Outputs [] prog5
                                  in (prog6, newScore, (newerOldX, newerOldY))

          (newerProg2, newerS, _) = head $ dropWhile (\(_, _, (ox, oy)) -> ox /= ballX || oy /= ballY) crushes

          -- new ball pos
          (newProg3, [newBallX, newBallY,_]) = collect3Outputs [] newerProg2
      in Playing newProg3 (newBallX, newBallY) newPaddle newerS



