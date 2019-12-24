module Day22 where

import Data.List
import qualified Data.Sequence as S
import Data.Maybe
import Data.Foldable
import qualified Data.Set as Set
import Math.NumberTheory.Powers.Modular

import Debug.Trace

data Command = DealIncrement Integer | DealNewStack | Cut Integer deriving Show

runCommand :: S.Seq Int -> Command -> S.Seq Int
runCommand stack (Cut i)
   | i > 0     = let (left, right) = S.splitAt (fromInteger i) stack in right S.>< left -- take i cards from the top and move to the bottom
   | i < 0     = let (left, right) = S.splitAt (length stack + fromInteger i) stack in right S.>< left -- take i cards from the bottom and move to the top
   | otherwise = stack
runCommand stack DealNewStack = S.reverse stack
runCommand stack (DealIncrement i) = let oldToNew = fmap (\index -> fitIntoSize (length stack) (fromInteger i * index)) [0..length stack - 1]
                                         mapping = zip (toList stack) oldToNew
                                         copy = stack in
                                     foldl (\newS (origVal, to) -> S.adjust (const origVal) to newS) copy mapping

fitIntoSize :: Int -> Int -> Int
fitIntoSize l i = i `rem` l

parse :: String -> Command
parse s
    | "deal with increment " `isPrefixOf` s = DealIncrement (read $ drop (length "deal with increment ") s)
    | "deal into new stack" `isPrefixOf` s = DealNewStack
    | "cut " `isPrefixOf` s = Cut (read $ drop (length "cut ") s)
    | otherwise = error s


part1 :: IO ()
part1 = do
    content <- readFile "resources/day22.txt"
    let commands = fmap parse (lines content)
    let stack = S.fromList [0..10006]
    let shuffled = foldl runCommand stack commands
    --putStrLn $ unwords $ toList $ fmap show (S.take 1500 (shuffled))
    --putStrLn $ "0 index: " ++ show (S.elemIndexL 0 shuffled)
    let solution = fromJust $ S.elemIndexL 2019 shuffled
    putStrLn $ "Solution: " ++ show solution -- 3939

reverseOutputs :: S.Seq Int -> Command -> S.Seq Int
reverseOutputs outputs command =
    let backMappings = zip [0..] $ fmap (\i -> runReverseCommand (fromIntegral $ length outputs) (fromIntegral i) command) [0..length outputs - 1]
        copy = outputs in
     foldl (\newS (from, to) -> S.adjust (\_ -> S.index outputs from) (fromInteger to) newS) copy backMappings

-- runReverseCommand will return the index in the previous stack that provided the value in the index given to this method
-- So
-- zip [0..] (fmap (\i -> runReverseCommand i command) [0..length seq - 1])
-- provides a mapping from the indices of runCommand to their parents in the original input
runReverseCommand :: Integer -> Integer -> Command -> Integer
runReverseCommand ln index (Cut iInt)
   | i > 0     = let offSetFromEnd = ln - i in if index >= offSetFromEnd then index - offSetFromEnd else index + i  -- take i cards from the top and move to the bottom
   | i < 0     = if index >= (-i) then index + i else index + (ln + i)   -- take i cards from the bottom and move to the top
   | otherwise = index
   where i = fromIntegral iInt
runReverseCommand ln index DealNewStack = ln - index - 1
runReverseCommand ln index (DealIncrement iInt) = let i = fromIntegral iInt
                                                      inverse = fromJust $ modInv i ln in
                                                  (inverse * index) `rem` ln

-- Given a and m, return Just x such that ax = 1 mod m.
-- If there is no such x return Nothing.
modInv :: Integer -> Integer -> Maybe Integer
modInv a m
  | 1 == g = Just (mkPos i)
  | otherwise = Nothing
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

-- input seems to be a combination of a single DealIncrement x followed by a single Cut y.
-- No idea what the logic for determining x and y is but we could find the increment by reversing the input commands and comparing output 0 with output 1.
-- from the increment and the division we could perhaps trace back the shift y.
-- output 0: 81917208448684
-- 1:        49379101137247
-- 2:        16840993825810
-- el 3:    103618604028420
-- el 4:     71080496716983
-- 49379101137247 - 16840993825810 = 32538107311437 : the increment offset(decreasing)
-- 49379101137247 `rem` 32538107311437 = 16840993825810: el. nr 2 was the last el of the serie and also the mod offset;
-- the increment = modInv incOffset length. So in this case, the increment was modInv 32538107311437 119315717514047 = 117095512549178
-- (val * increment) mod length = origIndex
-- (found value * inc) `rem` length = orig index:
-- (81917208448684 * 117095512549178) `rem` 119315717514047 = 51399895659261
-- (49379101137247 * 117095512549178) `rem` 119315717514047 = 51399895659260
-- So we get the elements in decreasing order, apparently there has been a reverse as well
-- (103618604028420 * 117095512549178) `rem` 119315717514047 = 51399895659258  .. etc.

-- So it seems that the input was:
-- DealIncrement 117095512549178
-- DealNewStack
-- Cut 51399895659261
-- Note that combined transformations return back to the original after sz - 1 iterations!
-- this suggests that the offset is a generator p*n mod length where n is the iteration
-- offset * (modInv-for-length n) should then give p.
-- Note: since inc at generation 0 is 1, we know for sure that p is inc (generation 1) because that value = p * 1
-- When we have this, we can determine the value for the increment after 101741582076661 repetitions: it is (inc1 ^ 101741582076661) mod 119315717514047
-- we need a modpower function to calc this.
-- then we still need to find the value for the cut because that changes as well, but this one also comes back to original so the calculation should be similar.
-- also sometimes the list is in reverse and sometimes not.
-- NOTE the inc calculation is correct, but in addition the range is alternating! e.g. 101 alternates 1, -1, -1, 1, -1, -1, 1 -> doesn't seem to follow a pattern ,..
-- so maybe just try both outcomes, the positive and the negative one:
-- SO: calc fast inc using modpower. Do 2 calculations: one with the positive inc  and increasing, one with the negative and decreasing (so reversed)
-- Calculate the cut:
-- take value at iteration n index 0
-- inc = calcN for generation n
-- (found value * inc) `rem` length = orig index = the cut
part2 :: IO ()
part2 = do
    content <- readFile "resources/day22.txt"
    let commands = fmap parse (lines content)
    let stackLength = 10007
    -- let calcSolutionAtIndex i = foldl (runReverseCommand stackLength) i (reverse commands) --2020
    -- we would have to iterate 101741582076661 times ..
    -- let calcNTimes n i = head $ drop n $ iterate (calcSolutionAtIndex commands stackLength) i
    --let calcSeqNTimes n sz = fmap (calcNTimes commands stackLength n) [0..sz-1]
    --putStrLn  "Seqs after 1 2 and 3 times:"
--    putStrLn $ unwords $ fmap show (calcSeqNTimes 1 stackLength)
--    putStrLn $ unwords $ fmap show (calcSeqNTimes 2 stackLength)
--    putStrLn $ unwords $ fmap show (calcSeqNTimes 3 stackLength)

-- TODO try some more series. Problems: the cut is sometimes negated, and perhaps there are also still reversed series
--      and the guessing of the final series for the answer doesn't seem to give good results even for gen 1 2 and 3

--    putStrLn "Incs: (from 1)"
--    let incs = fmap (calcIncAfterNTimes commands stackLength) [1..126]
--    putStrLn $ unwords $ fmap show incs
--
--    putStrLn "fast Incs: (from 1)"
--    let incs2 = fmap (fastCalcIncAfterNTimes commands stackLength) [1..126]
--    putStrLn $ unwords $ fmap show incs2

    putStrLn "correct Incs: (from 1)"
    let incs3 = fmap (fastCalcIncAfterNTimesCorrect commands stackLength) [1..stackLength-1]
    putStrLn $ take 30 $ unwords $ fmap show incs3
--
--    putStrLn " Incs signs:"
--    let signs = fmap (incSign commands stackLength) [0..99]
--    putStrLn $ unwords $ fmap show signs
--
--    putStrLn "Real cuts: (from 1)"
--    let cuts = take 30 $ fmap (calcCutAfterNTimesSlow commands stackLength) [1..126]
--    putStrLn $ unwords $ fmap show cuts

--    putStrLn " cuts: (from 1)"
--    let cuts = take 30 $ fmap (calcCutAfterNTimes commands stackLength) [1..126]
--    putStrLn $ unwords $ fmap show cuts

    -- putStrLn $ show (guessCutAfterNTimes commands stackLength 2)
    putStrLn "guessed cuts: (from 1)"
    let cuts2 = fmap (guessCutAfterNTimes commands stackLength) [1..(fromIntegral stackLength)-1]
    putStrLn $ take 30 $ unwords $ fmap show cuts2
--
--    putStrLn "slow cuts:"
--    let cuts2 = fmap (calcCutAfterNTimesSlow commands stackLength) [0..99]
--    putStrLn $ unwords $ fmap show cuts2

    -- seq after 1 time:
    putStrLn "Real Seq after 1:"
    putStrLn $ unwords $ take 30 $ fmap show (calcSeqNTimes commands stackLength 1)

    let guessedCommands1 = [DealIncrement (head incs3), Cut (stackLength - head cuts2)]
    putStrLn "Guessed Seq after 1:"
    putStrLn $ unwords $ take 30 $ fmap show (calcSeqNTimes guessedCommands1 stackLength 1)

    -- seq after 2 time:
    putStrLn "Real Seq after 2:"
    putStrLn $ unwords $ take 30 $ fmap show (calcSeqNTimes commands stackLength 2)

    let guessedCommands2 = [DealIncrement (head (tail incs3)), Cut (stackLength - head (tail cuts2))]
    -- TODO it should always be length - cut because that's when pos 0 is moved enough.
    putStrLn "Guessed Seq after 2:"
    putStrLn $ unwords $ take 30 $ fmap show (calcSeqNTimes guessedCommands2 stackLength 1)

       -- seq after 3 time:
    putStrLn "Real Seq after 3:"
    putStrLn $ unwords $ take 30 $ fmap show (calcSeqNTimes commands stackLength 3)

    let guessedCommands3 = [DealIncrement (head (tail $ tail incs3)), Cut (stackLength - head (tail $ tail cuts2))]
    putStrLn "Guessed Seq after 3:"
    putStrLn $ unwords $ take 30 $ fmap show (calcSeqNTimes guessedCommands3 stackLength 1)

    let finalInc = fastCalcIncAfterNTimesCorrect commands 119315717514047 (101741582076661-1)
    let finalCut = guessCutAfterNTimes commands 119315717514047 (101741582076661-1)
    putStrLn $ "Final Inc: " ++ show finalInc
    putStrLn $ "Final cut: " ++ show finalCut

    let finalCommands = [DealIncrement finalInc, Cut (119315717514047 - finalCut)]

    let solution = calcSolutionAtIndex finalCommands 119315717514047 2020
    putStrLn $ "Solution: " ++ show solution
--    let finalCommands2 = [DealIncrement finalInc, Cut (10007 - finalCut), DealNewStack] -- TODO also check the negating of the inc in case of decreasing series.
--    let solution2 = calcSolutionAtIndex finalCommands2 119315717514047 2020
--    putStrLn $ "Solution reversed: " ++ show solution2

incSign :: [Command] -> Integer -> Integer -> Integer
incSign commands stackLength n =
    let fast = fastCalcIncAfterNTimes commands stackLength n
        exact = calcIncAfterNTimes commands stackLength (fromInteger n)
    in if fast == exact then 1 else -1

pow:: Integer -> Integer -> Integer
pow a b = turboPower' 1 a b
  where
    turboPower' x a 0 = x
    turboPower' x a b
        | x `seq` a `seq` b `seq` False = undefined
        | even b = turboPower' x (a*a) (b `div` 2)
        | otherwise = turboPower' (x*a) a (b-1)

-- calc c after 1 times
-- calc c after 2 times
-- cut(n) = a*cut(n-1) + b
-- b = cut(1) - cut(0) because cut(0) is always 0
-- a = (cut(n) - b) * modInv (cut(n-1))   (modulo length again)

-- cut(n) = b * (a ^ (n-1) + a ^ (n-2) + ... 1)
-- this is a geometric serie which has a closed formula.
guessCutAfterNTimes :: [Command] -> Integer -> Int -> Integer
guessCutAfterNTimes commands stackLength n =
   let b = calcCutAfterNTimes commands stackLength 1
       cut2 = calcCutAfterNTimes commands stackLength 2
       a = (absMod (cut2 - b) stackLength) * fromJust (modInv b stackLength)
       am1Inv = fromJust $ modInv (a-1) stackLength
       cutN = (b * (powMod a n stackLength) - b * a) * am1Inv + b
   in (cutN `mod` stackLength)  -- TODO No idea why but it is either this or stackLength - this. Maybe always return 2 guesses? Or better, verify the guess somehow ..

absMod :: Integer -> Integer -> Integer
absMod x m = if x < 0 then m + x else x

-- Calculate the cut:
-- take value at iteration n index 0
-- inc = calcN for generation n
-- (found value * inc) `rem` length = orig index = the cut
calcCutAfterNTimes :: [Command] -> Integer -> Int -> Integer
calcCutAfterNTimes commands stackLength n =
   let inc = calcIncAfterNTimes commands stackLength n
       out0 = calcNTimes commands stackLength n 0
       sign = incSign commands stackLength (fromIntegral n)
       res = inc * out0 `mod` stackLength
   in if sign > 0 then (stackLength - res) `mod` stackLength else res

calcSeqNTimes :: [Command] -> Integer -> Int -> [Integer]
calcSeqNTimes commands stackLength n = fmap (calcNTimes commands stackLength n) [0..stackLength-1]

calcCutAfterNTimesSlow :: [Command] -> Integer -> Int -> Int
calcCutAfterNTimesSlow commands stackLength n =
   let seq = calcSeqNTimes commands stackLength n
   in fromJust $ elemIndex 0 seq

-- NOTE: the result is either correct or should be negated (mod stacklength)
fastCalcIncAfterNTimes :: [Command] -> Integer -> Integer -> Integer
fastCalcIncAfterNTimes commands stackLength n =
    let generator = calcIncrementGeneratingPrime commands stackLength
    in powMod generator n stackLength

calcIncrementGeneratingPrime :: [Command] -> Integer -> Integer
calcIncrementGeneratingPrime commands stackLength = calcIncAfterNTimes commands stackLength 1

calcIncAfterNTimes :: [Command] -> Integer -> Int -> Integer
calcIncAfterNTimes commands stackLength n =
  let out0 = calcNTimes commands stackLength n 0
      out1 = calcNTimes commands stackLength n 1
      offset = abs (out1 - out0)
  in fromJust $ modInv offset stackLength

calcIncCorrect :: [Command] -> Integer  -> Integer
calcIncCorrect commands stackLength =
   let cutAt1 = guessCutAfterNTimes commands stackLength 1
       oldIndex = calcNTimes commands stackLength 1 (cutAt1 + 1) -- TODO  or it can be stackLength - cutAt1 + 1. Maybe we could check both for just catAt1 to see which is 0?
   in fromJust $ modInv oldIndex stackLength

-- NOTE: the result is either correct or should be negated (mod stacklength)
fastCalcIncAfterNTimesCorrect :: [Command] -> Integer -> Integer -> Integer
fastCalcIncAfterNTimesCorrect commands stackLength n =
    let generator = calcIncCorrect commands stackLength
    in powMod generator n stackLength

calcNTimes :: [Command] -> Integer -> Int -> Integer -> Integer
calcNTimes commands stackLength n i = head $ drop n $ iterate (calcSolutionAtIndex commands stackLength) i

calcSolutionAtIndex :: [Command] -> Integer -> Integer -> Integer
calcSolutionAtIndex commands stackLength i = foldl (runReverseCommand stackLength) i (reverse commands)

--calcAndCheckMatch :: (Integer -> Integer) -> (Set.Set Integer, Integer, Bool) -> (Set.Set Integer, Integer, Bool)
--calcAndCheckMatch f (matches, i, _) = let newOutput = f i in
--                                      if Set.member newOutput matches then
--                                         trace "Found match!" (matches, newOutput, True)
--                                      else (Set.insert newOutput matches, newOutput, False)
