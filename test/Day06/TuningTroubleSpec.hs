module Day06.TuningTroubleSpec (spec) where

import Day06.TuningTrouble
import Test.Hspec

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

  describe "How many characters need to be processed before the first start-of-message marker is detected? (14 distinct characters)" $ do
    it "first marker after character 19" $ do
      findCorrectMarker "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` 19

    it "first marker after character 23" $ do
      findCorrectMarker "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 23

    it "first marker after character 23" $ do
      findCorrectMarker "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` 23

    it "first marker after character 29" $ do
      findCorrectMarker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 29

    it "first marker after character 26" $ do
      findCorrectMarker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` 26
