module UtilsSpec (spec) where

import Test.Hspec
import Utils

test_toNumber :: Spec
test_toNumber =
  describe "toNumber" $ 
    it "turns a list of digits into a number" $ do
      toNumber [1,2,3] `shouldBe` 123
      toNumber [0] `shouldBe` 0

test_bfs :: Spec
test_bfs =
  describe "bfs" $ do
    it "finds the the first candidate that satisfies the matchFunction" $
      bfs 1 (\x -> [x+1]) (\x -> even ((x + 1) `div` 2)) `shouldBe` Just 3
    it "returns Nothing if no path found" $
      bfs 0 (\x -> if x < 10 then [x+1] else []) (==11) `shouldBe` Nothing

test_BfsWithPath :: Spec
test_BfsWithPath =
  describe "bfsWithPath" $ do
    it "finds the shortest (reversed) path to a node satisfying the matchFunction" $ do
      bfsWithPath 1 (\x -> [x+1]) (==1) `shouldBe` Just [1]
      bfsWithPath 1 (\x -> [x+1]) (==3) `shouldBe` Just [3,2,1]
    it "returns Nothing if no path found" $
      bfsWithPath 0 (\x -> if x < 10 then [x+1] else []) (==11) `shouldBe` Nothing

spec :: Spec
spec = do
   test_toNumber
   test_bfs
   test_BfsWithPath

-- For running the test stand-alone
main :: IO ()
main = hspec spec
