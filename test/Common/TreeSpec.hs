module Common.TreeSpec (spec) where

import Common.Tree
import Test.Hspec
import Prelude hiding (length, lookup)

spec :: Spec
spec = do
  describe "When create empty tree" $ do
    let zero = empty :: Tree String Int

    it "Should have zero length" $ do
      length zero `shouldBe` 0

    it "Should have no elems" $ do
      elems zero `shouldBe` []

    it "Should be empty" $ do
      isEmpty zero `shouldBe` True

    it "Should NOT lookup anything" $ do
      lookup ["a"] zero `shouldBe` Nothing

    it "Should NOT have members" $ do
      member ["a"] zero `shouldBe` False

    it "Should NOT provide any subtree" $ do
      subtree ["a"] zero `shouldBe` empty

    it "Should provide subtrees" $ do
      subtrees zero `shouldBe` [zero]

    describe "When inserting an element to root level" $ do
      let one = insert ["a"] 123 zero

      it "Should have length" $ do
        length one `shouldBe` 1

      it "Should have elems" $ do
        elems one `shouldBe` [123]

      it "Should have elems" $ do
        keys one `shouldBe` [["a"]]

      it "Should be empty" $ do
        isEmpty one `shouldBe` False

      it "Should lookup a member" $ do
        lookup ["a"] one `shouldBe` Just 123

      it "Should NOT lookup a anything else" $ do
        lookup ["b"] one `shouldBe` Nothing

      it "Should have members" $ do
        member ["a"] one `shouldBe` True

      it "Should NOT have non-inserted members" $ do
        member ["b"] one `shouldBe` False

      it "Should provide empty subtree for 'a'" $ do
        subtree ["a"] one `shouldBe` empty

      it "Should NOT provide any other subtrees" $ do
        subtree ["b"] one `shouldBe` empty

      it "Should provide subtrees" $ do
        subtrees one `shouldBe` [one]

      describe "When inserting a second element to root level" $ do
        let two = insert ["b"] 342 one

        it "Should have length" $ do
          length two `shouldBe` 2

        it "Should have keys" $ do
          keys two `shouldBe` [["a"], ["b"]]

        it "Should have elems" $ do
          elems two `shouldBe` [123, 342]

        it "Should be empty" $ do
          isEmpty two `shouldBe` False

        it "Should lookup a member 'a'" $ do
          lookup ["a"] two `shouldBe` Just 123

        it "Should lookup a member 'b'" $ do
          lookup ["b"] two `shouldBe` Just 342

        it "Should NOT lookup a anything else" $ do
          lookup ["c"] two `shouldBe` Nothing

        it "Should have members" $ do
          member ["a"] two `shouldBe` True

        it "Should have members" $ do
          member ["b"] two `shouldBe` True

        it "Should NOT have non-inserted members" $ do
          member ["c"] two `shouldBe` False

        it "Should provide empty subtree for 'a'" $ do
          subtree ["a"] two `shouldBe` empty

        it "Should provide empty subtree for 'b'" $ do
          subtree ["b"] two `shouldBe` empty

        it "Should NOT provide any other subtrees" $ do
          subtree ["c"] two `shouldBe` empty

        it "Should provide subtrees" $ do
          subtrees two `shouldBe` [two]

        describe "When inserting a first element to second level" $ do
          let three = insert ["c", "d"] 942 two

          it "Should have one length" $ do
            length three `shouldBe` 3

          it "Should have keys" $ do
            keys three `shouldBe` [["a"], ["b"], ["c", "d"]]

          it "Should have no elems" $ do
            elems three `shouldBe` [123, 342, 942]

          it "Should be empty" $ do
            isEmpty three `shouldBe` False

          it "Should lookup a member 'a'" $ do
            lookup ["a"] three `shouldBe` Just 123

          it "Should lookup a member 'b'" $ do
            lookup ["b"] three `shouldBe` Just 342

          it "Should lookup a second level member" $ do
            lookup ["c", "d"] three `shouldBe` Just 942

          it "Should NOT lookup a anything else" $ do
            lookup ["c", "e"] three `shouldBe` Nothing

          it "Should have members" $ do
            member ["a"] three `shouldBe` True

          it "Should have members" $ do
            member ["b"] three `shouldBe` True

          it "Should have second-level members" $ do
            member ["c", "d"] three `shouldBe` True

          it "Should NOT have non-inserted members" $ do
            member ["c", "e"] three `shouldBe` False

          it "Should provide subtree for 'a'" $ do
            subtree ["c"] three `shouldBe` insert ["d"] 942 empty

          it "Should provide empty subtree for 'b'" $ do
            subtree ["b"] three `shouldBe` empty

          it "Should provide empty subtree for 'c/d'" $ do
            subtree ["c", "d"] three `shouldBe` empty

          it "Should NOT provide any other subtrees" $ do
            subtree ["d"] three `shouldBe` empty

          it "Should provide subtrees" $ do
            subtrees three `shouldBe` [three, insert ["d"] 942 empty]

          describe "When inserting a first element to third level" $ do
            let four = insert ["c", "e", "f"] 754 three

            it "Should have one length" $ do
              length four `shouldBe` 4

            it "Should have keys" $ do
              keys four `shouldBe` [["a"], ["b"], ["c", "d"], ["c", "e", "f"]]

            it "Should have no elems" $ do
              elems four `shouldBe` [123, 342, 942, 754]

            it "Should be empty" $ do
              isEmpty four `shouldBe` False

            it "Should lookup a member 'a'" $ do
              lookup ["a"] four `shouldBe` Just 123

            it "Should lookup a member 'b'" $ do
              lookup ["b"] four `shouldBe` Just 342

            it "Should lookup a second level member" $ do
              lookup ["c", "d"] four `shouldBe` Just 942

            it "Should lookup a third level member" $ do
              lookup ["c", "e", "f"] four `shouldBe` Just 754

            it "Should NOT lookup a anything else" $ do
              lookup ["f", "e", "g"] four `shouldBe` Nothing

            it "Should have members" $ do
              member ["a"] four `shouldBe` True

            it "Should have members" $ do
              member ["b"] four `shouldBe` True

            it "Should have second-level members" $ do
              member ["c", "d"] four `shouldBe` True

            it "Should have third-level members" $ do
              member ["c", "e", "f"] four `shouldBe` True

            it "Should NOT have non-inserted members" $ do
              member ["a", "b", "d"] four `shouldBe` False

            it "Should provide subtree for 'c'" $ do
              subtree ["c"] four `shouldBe` (insert ["e", "f"] 754 . insert ["d"] 942 $ empty)

            it "Should provide empty subtree for 'b'" $ do
              subtree ["b"] four `shouldBe` empty

            it "Should provide subtree for 'c/e'" $ do
              subtree ["c", "e"] four `shouldBe` insert ["f"] 754 empty

            it "Should NOT provide any other subtrees" $ do
              subtree ["d"] four `shouldBe` empty

            it "Should provide subtrees" $ do
              subtrees four `shouldBe` [four, fromList [(["d"], 942), (["e", "f"], 754)], fromList [(["f"], 754)]]

            describe "When inserting a second element to third level" $ do
              let five = insert ["f", "c", "e"] 870 four

              it "Should have one length" $ do
                length five `shouldBe` 5

              it "Should have keys" $ do
                keys five `shouldBe` [["a"], ["b"], ["c", "d"], ["c", "e", "f"], ["f", "c", "e"]]

              it "Should have no elems" $ do
                elems five `shouldBe` [123, 342, 942, 754, 870]

              it "Should be empty" $ do
                isEmpty five `shouldBe` False

              it "Should lookup a member 'a'" $ do
                lookup ["a"] five `shouldBe` Just 123

              it "Should lookup a member 'b'" $ do
                lookup ["b"] five `shouldBe` Just 342

              it "Should lookup a second level member" $ do
                lookup ["c", "d"] five `shouldBe` Just 942

              it "Should lookup a third level member" $ do
                lookup ["c", "e", "f"] five `shouldBe` Just 754

              it "Should lookup an other third level member" $ do
                lookup ["f", "c", "e"] five `shouldBe` Just 870

              it "Should NOT lookup a anything else" $ do
                lookup ["a", "d", "c"] five `shouldBe` Nothing

              it "Should have members" $ do
                member ["a"] five `shouldBe` True

              it "Should have members" $ do
                member ["b"] five `shouldBe` True

              it "Should have second-level members" $ do
                member ["c", "d"] five `shouldBe` True

              it "Should have third-level members" $ do
                member ["c", "e", "f"] five `shouldBe` True

              it "Should have other third-level members" $ do
                member ["f", "c", "e"] five `shouldBe` True

              it "Should NOT have non-inserted members" $ do
                member ["f", "c", "z"] five `shouldBe` False

              it "Should read fromList" $ do
                fromList [(["a"], 123), (["b"], 342), (["c", "d"], 942), (["c", "e", "f"], 754), (["f", "c", "e"], 870)] `shouldBe` five

              it "Should provide subtrees" $ do
                subtrees five
                  `shouldBe` [ five,
                               fromList [(["d"], 942), (["e", "f"], 754)],
                               fromList [(["f"], 754)],
                               fromList [(["c", "e"], 870)],
                               fromList [(["e"], 870)]
                             ]
