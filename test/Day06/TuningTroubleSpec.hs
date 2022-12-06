module Day06.TuningTroubleSpec (spec) where

import Test.Hspec
import Day06.TuningTrouble

spec :: Spec
spec = do
  describe "How many characters need to be processed before the first start-of-packet marker is detected?" $ do
    it "first marker after character 5" $ do
      findFirstMarker "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 5
      
    it "first marker after character 6" $ do
      findFirstMarker "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` 6
      
    it "first marker after character 10" $ do
      findFirstMarker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 10
      
    it "first marker after character 11" $ do
      findFirstMarker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` 11
