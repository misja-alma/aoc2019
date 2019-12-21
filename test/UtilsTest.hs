module UtilsTest where

import SpecHelper
import Test.Hspec
import Utils

test_toNumber :: Spec
test_toNumber =
  describe "toNumber" $ 
    it "turns a list of digits into a number" $
      toNumber [1,2,3] `shouldBe` 123

test_Bfs :: Spec
test_Bfs =
  describe "bfs" $ 
    it "finds the shortest (reversed) path to a node satisfying the matchFunction" $
      bfs 1 (+1) (==3) `shouldBe` [3,2,1]
      
      
main :: IO ()
main = hspec $ do
         test_toNumber     
         test_Bfs 