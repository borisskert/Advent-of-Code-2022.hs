module Day10.InstructionSpec (spec) where

import Test.Hspec
import Day10.Instruction

spec :: Spec
spec = do
  describe "When reading Instructions" $ do
    it "Should read noop" $ do
      (show . (read :: String -> Instruction) $ "noop") `shouldBe` "Noop" 
    
    it "Should read addX 1" $ do
      (show . (read :: String -> Instruction) $ "addX 1") `shouldBe` "AddX 1" 
