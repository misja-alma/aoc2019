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
-- it turns out that when repeated (stackLength-1) times, this combination gives back the original series
-- So it must be that the combined commands lead up to a linear transformation of the form inew = a*iold + b
-- repeating this transformation gives in = a^n * i0 + b(a^(n-1) + a^(n-2) ... + 1)
-- the right term (B) is a geometric series that has a closed form, the left part (A) can be calcuated with modular exponentiation
-- then: in = A*i0 + B
-- i0 = modInv (A) * (in - B)
part2 :: IO ()
part2 = do
    content <- readFile "resources/day22.txt"
    let commands = fmap parse (lines content)
    let stackLength = 10007

    -- seq after 1 time:
    putStrLn "Real Seq after 1:"
    putStrLn $ unwords $ take 30 $ fmap show (calcSeqNTimes commands stackLength 1)

    putStrLn "New Seq after 1:"
    putStrLn $ unwords $ take 30 $ fmap show (fmap (reverseCalcPositionAtGen commands stackLength 1) [0..stackLength-1])
    putStrLn ""

    -- seq after 2 time:
    putStrLn "Real Seq after 2:"
    putStrLn $ unwords $ take 30 $ fmap show (calcSeqNTimes commands stackLength 2)

    putStrLn "New Seq after 2:"
    putStrLn $ unwords $ take 30 $ fmap show (fmap (reverseCalcPositionAtGen commands stackLength 2) [0..stackLength-1])
    putStrLn ""

       -- seq after 3 time:
    putStrLn "Real Seq after 3:"
    putStrLn $ unwords $ take 30 $ fmap show (calcSeqNTimes commands stackLength 3)

    putStrLn "New Seq after 3"
    putStrLn $ unwords $ take 30 $ fmap show (fmap (reverseCalcPositionAtGen commands stackLength 3) [0..stackLength-1])
    putStrLn ""

    let solution = reverseCalcPositionAtGen commands 119315717514047 101741582076661 2020
    putStrLn $ "Solution: " ++ show solution

-- returns the position at gen0 that contained the element now at position i in gen n. Which is also the value if at gen 0 the stack was [0..]
reverseCalcPositionAtGen :: [Command] -> Integer -> Integer -> Integer -> Integer
reverseCalcPositionAtGen commands stackLength n i =
  let val0 = calcNTimes commands stackLength 1 0
      val1 = calcNTimes commands stackLength 1 1
      a = fromJust $ modInv (absMod (val1 - val0) stackLength) stackLength
      b = absMod (-a * val0) stackLength
      am1Inv = fromJust $ modInv (a-1) stackLength
      aAfterN = powMod a n stackLength
      bAfterN = (b * aAfterN - b * a) * am1Inv + b
      res = fromJust (modInv aAfterN stackLength) * (i - bAfterN) --  trace ("a: " ++ show a ++ " b " ++ show b ++ " a after n " ++ show aAfterN) $
  in res `mod` stackLength

pow:: Integer -> Integer -> Integer
pow a b = turboPower' 1 a b
  where
    turboPower' x a 0 = x
    turboPower' x a b
        | x `seq` a `seq` b `seq` False = undefined
        | even b = turboPower' x (a*a) (b `div` 2)
        | otherwise = turboPower' (x*a) a (b-1)

absMod :: Integer -> Integer -> Integer
absMod x m = if x < 0 then m + x else x


calcSeqNTimes :: [Command] -> Integer -> Int -> [Integer]
calcSeqNTimes commands stackLength n = fmap (calcNTimes commands stackLength n) [0..stackLength-1]

calcNTimes :: [Command] -> Integer -> Int -> Integer -> Integer
calcNTimes commands stackLength n i = head $ drop n $ iterate (calcSolutionAtIndex commands stackLength) i

calcSolutionAtIndex :: [Command] -> Integer -> Integer -> Integer
calcSolutionAtIndex commands stackLength i = foldl (runReverseCommand stackLength) i (reverse commands)

