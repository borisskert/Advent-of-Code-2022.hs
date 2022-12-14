module Day10.ProgramSpec (spec) where

import Test.Hspec
import Day10.Program

exampleInput :: String
exampleInput = "noop\naddx 3\naddx -5"

spec :: Spec
spec = do
  describe "When reading program" $ do
    it "Should read small example input" $ do
      (show . (read :: String -> Program) $ exampleInput) `shouldBe` "Noop,AddX 3,AddX (-5)"
