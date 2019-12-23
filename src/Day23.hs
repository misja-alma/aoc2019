module Day23 where

import Data.List.Split
import Data.Array
import Data.List
import Data.Function
import Data.Char
import Utils
import Data.Maybe
import Debug.Trace
import qualified Data.Sequence as S
import Data.Foldable
import Data.Either

type Index = Int
type Base = Index
type Program = (Index, Base, Array Index Integer)
type Input = [Integer]
type Output = Integer
data ProgramState = Running Program Input | WaitForOutput Program Input Output | Halted | WaitForInput Program
type Operation = Input -> Program -> ProgramState
type Opcode = Integer

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
    if null inp then WaitForInput prog
    else
      let newAr = write prog 1 opcode (head inp) in
      Running (ip + 2, base, newAr) (tail inp)

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

execute :: Input -> Program -> ProgramState
execute input prog@(pc, base, ar) =
    let instr = ar ! pc
        operation = translate instr
        result = operation input prog in
    case result of
        Running newProg newInput -> execute newInput newProg
        outputOrHalted           -> outputOrHalted

collectOutputs :: Input -> Program -> (Program, [Output])
collectOutputs input program = case execute input program of
    Halted                        -> (program, [])
    WaitForInput prog             -> (prog, [])
    WaitForOutput prog inp outp   -> let (newProg, outputs) = collectOutputs inp prog in (newProg, outp : outputs)

-- address, x, y
data Packet = Packet Int Integer Integer deriving Show

type InputQueue = S.Seq (Integer, Integer)

commandsToInput :: [String] -> [Integer]
commandsToInput cs = cs >>= (\s -> fromIntegral <$> (fmap ord s ++ [10]))

toPacket :: [Output] -> Packet
toPacket [addr, x, y] = Packet (fromInteger addr) x y
toPacket _ = error "Invalid packet input"

toPackets :: [Output] -> [Packet]
toPackets outputs = toPacket <$> sliding 3 3 outputs

deliverPacket :: S.Seq InputQueue -> Packet -> Either Integer (S.Seq InputQueue)
deliverPacket queues (Packet addr x y) =
    if addr == 255 then Left y
    else let q = S.index queues addr
         in Right (S.adjust (const ((x,y) S.<| q)) addr queues)

-- Just delivers, doesn't let the progs read it yet
deliverPackets :: S.Seq InputQueue -> [Packet] -> Either Integer (S.Seq InputQueue)
deliverPackets qs = foldl (\rqs p -> if isLeft rqs then rqs else deliverPacket (safeFromRight rqs) p) (Right qs)

initialiseProg :: (S.Seq InputQueue, S.Seq Program) -> Int -> (S.Seq InputQueue, S.Seq Program)
initialiseProg (queues, progs) index =
    let prog = S.index progs index
        q = S.index queues index
        input = [fromIntegral index, -1]
        (updatedProg, output) = collectOutputs input prog
        Right updatedQueues = queues `seq` deliverPackets queues (toPackets output)
    in  updatedQueues `seq` (updatedQueues, S.adjust (const updatedProg) index progs)

-- Note that the queue needs to be read in reverse
toInputs :: S.Seq (Integer, Integer) -> Input
toInputs is = if S.null is then [-1] else concatMap (\(x, y) -> [x, y]) $ reverse $ toList is

sendAndReceiveFor :: (S.Seq InputQueue,S.Seq Program) -> Int -> Either Integer (S.Seq InputQueue, S.Seq Program)
sendAndReceiveFor (queues, progs) index =
    let q = queues `seq` S.index queues index
        prog = S.index progs index
        (newProg, output) = collectOutputs (toInputs q) prog
        emptyOwnQueue = S.adjust (const S.empty) index queues
        updatedProgs = S.adjust (const newProg) index progs
        updatedQueues = emptyOwnQueue `seq` deliverPackets emptyOwnQueue (toPackets output)
    in case updatedQueues of
          Left s   -> Left s
          Right qs -> Right (qs, updatedProgs)

safeFromRight :: Either a b -> b
safeFromRight e = case e of
                    Right b -> b
                    _       -> error "Right expected"

showQueue :: (Int, InputQueue) -> String
showQueue (i, q) = (show i ) ++ ": " ++ unwords (fmap show (toList q))

showQueues :: S.Seq InputQueue -> String
showQueues qs = unlines $ fmap showQueue (zip [0..] $ toList qs)

sendAndReceive :: Either Integer (S.Seq InputQueue, S.Seq Program) -> Either Integer (S.Seq InputQueue, S.Seq Program)
sendAndReceive rqs = rqs `seq`
    case rqs of
       Left s -> Left s
       Right qp -> foldl (\rq index -> if isRight rq then sendAndReceiveFor (safeFromRight rq) index else rq) rqs [0..49]

part1 :: IO ()
part1 = do
    content <- readFile "resources/day23.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let buffer = replicate 1000 0
    let opArray = array (0, length numbers + length buffer  - 1) $ zip [0..] (numbers ++ buffer)
    let program = (0,0,opArray)
    -- 50 programs, each program identified by its index
    -- each has their own input queue
    let progs = S.fromList $ replicate 50 program
    let queues = S.fromList $ replicate 50 S.empty
    -- for each prog, initialise it with its index and handle its first output commands
    let (newQueues, initialisedProgs) = foldl initialiseProg (queues, progs) [0..49]
    -- loop over progs, feeding each its input (or -1) and emptying it, delivering its output, until a packet to address 255 is sent
    let initialState = newQueues `seq` Right (newQueues, initialisedProgs)
    let Left solution = initialisedProgs `seq` head $ dropWhile isRight $ iterate sendAndReceive initialState
    putStrLn $ "Solution: " ++ show solution

part2 :: IO ()
part2 = do
    content <- readFile "resources/day23.txt"
    let numbers = (read :: String -> Integer) <$> splitOn "," content
    let buffer = replicate 1000 0
    let opArray = array (0, length numbers + length buffer  - 1) $ zip [0..] (numbers ++ buffer)
    let program = (0,0,opArray)
    -- 50 programs, each program identified by its index
    -- each has their own input queue
    let progs = S.fromList $ replicate 50 program
    let queues = S.fromList $ replicate 50 S.empty
    -- for each prog, initialise it with its index and handle its first output commands
    let (newQueues, initialisedProgs) = foldl initialiseProg (queues, progs) [0..49]
    -- loop over progs, feeding each its input (or -1) and emptying it, delivering its output, until a packet to address 255 is sent
    let initialState = newQueues `seq` Right (NAT [] Nothing, newQueues, initialisedProgs)
    let Left solution = initialisedProgs `seq` head $ dropWhile isRight $ iterate sendAndReceive2 initialState
    putStrLn $ "Solution: " ++ show solution

-- previous ys, current packet (x,y)
data NAT = NAT [Integer] (Maybe (Integer, Integer))

checkFinished :: NAT -> Maybe Integer
checkFinished (NAT ys _) = if (length ys >= 2) && (head ys == (ys !! 1)) then Just (head ys) else Nothing

lastPacket :: NAT -> (Integer, Integer)
lastPacket (NAT _ p) =
    case p of
       Just xy    -> xy
       Nothing    -> error "last packet expected"

updateNat :: (Integer, Integer) -> NAT -> NAT
updateNat xy (NAT ys _) = NAT ys (Just xy)

updateAfterSending :: NAT -> NAT
updateAfterSending (NAT ys maybeLast) =
    case maybeLast of
        Nothing -> error "xy expected after sending"
        Just (x,y) -> NAT (y : ys) maybeLast

deliverPacket2 :: (NAT, S.Seq InputQueue) -> Packet -> (NAT, S.Seq InputQueue)
deliverPacket2 (nat, queues) (Packet addr x y) =
    if addr == 255 then (updateNat (x,y) nat, queues)
    else let q = S.index queues addr
         in (nat, S.adjust (const ((x,y) S.<| q)) addr queues)

-- Just delivers, doesn't let the progs read it yet
deliverPackets2 :: NAT -> S.Seq InputQueue -> [Packet] -> (NAT, S.Seq InputQueue)
deliverPackets2 nat qs = foldl deliverPacket2 (nat, qs)

sendAndReceive2For :: ((NAT,S.Seq InputQueue,S.Seq Program), Bool) -> Int -> ((NAT,S.Seq InputQueue, S.Seq Program), Bool)
sendAndReceive2For ((nat, queues, progs), sentSoFar) index =
    let q = queues `seq` S.index queues index
        prog = S.index progs index
        (newProg, output) = collectOutputs (toInputs q) prog
        newSent = sentSoFar || (not $ null output)
        emptyOwnQueue = S.adjust (const S.empty) index queues
        updatedProgs = S.adjust (const newProg) index progs
        (updatedNat, updatedQueues) = emptyOwnQueue `seq` deliverPackets2 nat emptyOwnQueue (toPackets output)
    in  ((updatedNat, updatedQueues, updatedProgs), newSent)

-- keep a NAT object as well that
-- keeps last 2 255's.
-- if both the same, responds with finished and the y value
-- otherwise keeps waiting until during a full iteration no output was sent. In that case it will send its kept packet to address 0.
sendAndReceive2 :: Either Integer (NAT, S.Seq InputQueue, S.Seq Program) -> Either Integer (NAT, S.Seq InputQueue, S.Seq Program)
sendAndReceive2 rqs =
    case rqs of
       Left s -> Left s
       Right nqp@(nat, qs, ps) ->
           let ((newNat, newQs, newPs), anythingSent) = foldl sendAndReceive2For (nqp, False) [0..49]
           in case checkFinished newNat of
                Just r -> Left r
                Nothing -> if anythingSent then Right (newNat, newQs, newPs)
                           else let queue0 = S.index newQs 0
                                    updatedQueue0 = lastPacket newNat S.<| queue0
                                    updatedQueues = S.adjust (const updatedQueue0) 0 newQs
                                in Right (updateAfterSending newNat, updatedQueues, newPs)
