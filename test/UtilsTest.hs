module UtilsTest (test_toNumber, test_bfs, test_BfsWithPath) where

import SpecHelper
import Test.Hspec
import Utils

test_toNumber :: Spec
test_toNumber =
  describe "toNumber" $ 
    it "turns a list of digits into a number" $
      toNumber [1,2,3] `shouldBe` 123

test_bfs :: Spec
test_bfs =
  describe "bfs" $
    it "finds the the first candidate that satisfies the matchFunction" $
      bfs 1 (\x -> [x+1]) (\x -> even ((x + 1) `div` 2)) `shouldBe` Just 3

test_BfsWithPath :: Spec
test_BfsWithPath =
  describe "bfsWithPath" $
    it "finds the shortest (reversed) path to a node satisfying the matchFunction" $
      bfsWithPath 1 (\x -> [x+1]) (==3) `shouldBe` Just [3,2,1]

-- TODO I don't think this is needed
main :: IO ()
main = hspec $ do
         test_toNumber
         test_bfs
         test_BfsWithPath
