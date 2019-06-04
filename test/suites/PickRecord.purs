module Test.Suites.PickRecord
  ( suites
  ) where

import Prelude (discard, pure, unit, ($))

import Record.Builder (build)
import Record.Extra.PickRecord (pickRecord)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)

suites :: TestSuite
suites =
  suite "PickRecord" do
    suite "Builder" do
      test "#0" do
        let input = {}
        build pickRecord input `shouldEqual` {}

      test "#1" do
        let input = { a0: 0 }
        build pickRecord input `shouldEqual` {}

      test "#2" do
        let input = { a0: 0 }
        build pickRecord input `shouldEqual` input

      test "#3" do
        let input = { a0: 0, a1: 1, a2: 2 }
        build pickRecord input `shouldEqual` { a1: 1, a2: 2 }

      test "#4" do
        let input = { a0: 0, a1: 1, a2: 2 }
        build pickRecord input `shouldEqual` { a0: 0, a2: 2 }

      test "#5 -- Should Not Compile" $ pure unit
--         let input = { a0: 0, a1: 1, a2: 2 }
--         build (pickRecord <<< pickRecord) input
--           `shouldEqual`
--           { a0: 0, a2: 2 }

      test "#6 -- Should Not Compile" $ pure unit
--         let input = { a0: 0, a1: 1, a2: 2 }
--         build (pickRecord <<< identity <<< pickRecord) input
--           `shouldEqual`
--           { a0: 0, a2: 2 }

      test "#7 -- Should Not Compile" $ pure unit
--         let input = {}
--         build pickRecord input `shouldEqual` { a0: 0 }

      test "#8" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3, a4: 4, a5: 5 }
        build pickRecord input
          `shouldEqual`
          { a5: 5, a3: 3, a1: 1, a0: 0, a2: 2, a4: 4 }

    suite "Function" do
      test "#0" do
        let input = {}
        pickRecord input `shouldEqual` {}

      test "#1" do
        let input = { a0: 0 }
        pickRecord input `shouldEqual` {}

      test "#2" do
        let input = { a0: 0 }
        pickRecord input `shouldEqual` input

      test "#3" do
        let input = { a0: 0, a1: 1, a2: 2 }
        pickRecord input `shouldEqual` { a1: 1, a2: 2 }

      test "#4" do
        let input = { a0: 0, a1: 1, a2: 2 }
        pickRecord input `shouldEqual` { a0: 0, a2: 2 }

      test "#5 -- Should Not Compile" $ pure unit
--         let input = { a0: 0, a1: 1, a2: 2 }
--         (pickRecord <<< pickRecord) input `shouldEqual` { a0: 0, a2: 2 }

      test "#6 -- Should Not Compile" $ pure unit
--         let input = { a0: 0, a1: 1, a2: 2 }
--         (pickRecord <<< identity <<< pickRecord) input
--           `shouldEqual`
--           { a0: 0, a2: 2 }

      test "#7 -- Should Not Compile" $ pure unit
--         let input = {}
--         pickRecord input `shouldEqual` { a0: 0 }

      test "#8" do
        let input = { a0: 0, a1: 1, a2: 2, a3: 3, a4: 4, a5: 5 }
        pickRecord input
          `shouldEqual`
          { a5: 5, a3: 3, a1: 1, a0: 0, a2: 2, a4: 4 }
