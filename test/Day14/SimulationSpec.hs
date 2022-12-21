module Day14.SimulationSpec (spec) where

import Common.Fold
import Common.Grid (fromTuple)
import Day14.Reservoir (empty, insertScans, withGround)
import Day14.Simulation
import Test.Hspec

exampleInput :: String
exampleInput = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9\n"

spec :: Spec
spec = do
  describe "When start simulation" $ do
    let simulation = from . (`insertScans` empty) . read $ exampleInput

    it "Should have no sand" $ do
      (length . sandUnits $ simulation) `shouldBe` 0

    it "Should determine drop point at column 500" $ do
      dropPoint simulation `shouldBe` (Just . fromTuple $ (500, 8))

    describe "When simulating sand unit's drop #1" $ do
      let one = oneDrop simulation
      let sandUnitsOne = sandUnits one

      it "Should have one more sand unit" $ do
        length sandUnitsOne `shouldBe` 1

      it "Should drop oneUnit" $ do
        sandUnitsOne `shouldBe` [fromTuple (500, 8)]

      describe "When simulating sand unit's drop #2" $ do
        let two = oneDrop one
        let sandUnitsTwo = sandUnits two

        it "Should have one more sand unit" $ do
          length sandUnitsTwo `shouldBe` 2

        it "Should drop oneUnit" $ do
          sandUnitsTwo `shouldBe` [fromTuple (500, 8), fromTuple (499, 8)]

        describe "When simulating sand unit's drop #3" $ do
          let three = oneDrop two
          let sandUnitsThree = sandUnits three

          it "Should have one more sand unit" $ do
            length sandUnitsThree `shouldBe` 3

          it "Should drop oneUnit" $ do
            sandUnitsThree `shouldBe` [fromTuple (500, 8), fromTuple (499, 8), fromTuple (501, 8)]

          describe "When simulating sand unit's drop #5" $ do
            let five = oneDrop . oneDrop $ three
            let sandUnitsFive = sandUnits five

            it "Should have one more sand unit" $ do
              length sandUnitsFive `shouldBe` 5

            it "Should drop oneUnit" $ do
              sandUnitsFive `shouldBe` [fromTuple (500, 8), fromTuple (499, 8), fromTuple (501, 8), fromTuple (500, 7), fromTuple (498, 8)]

            describe "When simulating sand unit's drop #22" $ do
              let ten = oneDrop . oneDrop . oneDrop . oneDrop . oneDrop $ five
              let drop20 = oneDrop . oneDrop . oneDrop . oneDrop . oneDrop . oneDrop . oneDrop . oneDrop . oneDrop . oneDrop $ ten
              let drop22 = oneDrop . oneDrop $ drop20
              let sandUnits22 = sandUnits drop22

              it "Should have one more sand unit" $ do
                length sandUnits22 `shouldBe` 22

              it "Should drop oneUnit" $ do
                sandUnits22
                  `shouldBe` [ fromTuple (500, 8),
                               fromTuple (499, 8),
                               fromTuple (501, 8),
                               fromTuple (500, 7),
                               fromTuple (498, 8),
                               fromTuple (499, 7),
                               fromTuple (501, 7),
                               fromTuple (500, 6),
                               fromTuple (497, 8),
                               fromTuple (498, 7),
                               fromTuple (499, 6),
                               fromTuple (501, 6),
                               fromTuple (500, 5),
                               fromTuple (499, 5),
                               fromTuple (501, 5),
                               fromTuple (500, 4),
                               fromTuple (499, 4),
                               fromTuple (501, 4),
                               fromTuple (500, 3),
                               fromTuple (499, 3),
                               fromTuple (501, 3),
                               fromTuple (500, 2)
                             ]
              it "Should show simulation state" $ do
                show drop22
                  `shouldBe` "......+...\n..........\n......o...\n.....ooo..\n....#ooo##\n....#ooo#.\n..###ooo#.\n....oooo#.\n...ooooo#.\n#########."

              describe "When simulating sand unit's drop #24" $ do
                let drop24 = oneDrop . oneDrop $ drop22
                let sandUnits24 = sandUnits drop24

                it "Should have one more sand unit" $ do
                  length sandUnits24 `shouldBe` 24

                it "Should drop oneUnit" $ do
                  sandUnits24
                    `shouldBe` [ fromTuple (500, 8),
                                 fromTuple (499, 8),
                                 fromTuple (501, 8),
                                 fromTuple (500, 7),
                                 fromTuple (498, 8),
                                 fromTuple (499, 7),
                                 fromTuple (501, 7),
                                 fromTuple (500, 6),
                                 fromTuple (497, 8),
                                 fromTuple (498, 7),
                                 fromTuple (499, 6),
                                 fromTuple (501, 6),
                                 fromTuple (500, 5),
                                 fromTuple (499, 5),
                                 fromTuple (501, 5),
                                 fromTuple (500, 4),
                                 fromTuple (499, 4),
                                 fromTuple (501, 4),
                                 fromTuple (500, 3),
                                 fromTuple (499, 3),
                                 fromTuple (501, 3),
                                 fromTuple (500, 2),
                                 fromTuple (497, 5),
                                 fromTuple (495, 8)
                               ]

                it "Should show simulation state" $ do
                  show drop24
                    `shouldBe` "......+...\n..........\n......o...\n.....ooo..\n....#ooo##\n...o#ooo#.\n..###ooo#.\n....oooo#.\n.o.ooooo#.\n#########."

    describe "When drop all until falling into abyss" $ do
      let alldropped = dropAll simulation
      let sandUnitsAll = sandUnits alldropped

      it "Should drop 24 sand units" $ do
        length sandUnitsAll `shouldBe` 24

  describe "When drop allunits in a reservoir with ground" $ do
    let simulation = from . withGround . (`insertScans` empty) . read $ exampleInput
    let droppedAll = times oneDrop simulation 93

    it "Should show" $ do
      show droppedAll `shouldBe` "...........o...........\n..........ooo..........\n.........ooooo.........\n........ooooooo........\n.......oo#ooo##o.......\n......ooo#ooo#ooo......\n.....oo###ooo#oooo.....\n....oooo.oooo#ooooo....\n...oooooooooo#oooooo...\n..ooo#########ooooooo..\n.ooooo.......ooooooooo.\n#######################"
