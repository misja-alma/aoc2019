module UtilsTest where

import Test.Hspec
import Utils

-- TODO Figure out how to get cabal test to work. For now, run in cabal repl with 'hspec spec'. 
spec :: Spec
spec = 
  describe "toNumber" $ 
    it "turns a list of digits into a number" $
      toNumber [1,2,3] `shouldBe` 123