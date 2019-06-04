module Test.Suites.PickRecord
  ( suites
  ) where

import Prelude (discard)

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

--       test "#1" do
--         let input = { a0: 0 }
--         build pickRecord input `shouldEqual` {}

      test "#2" do
        let input = { a0: 0 }
        build pickRecord input `shouldEqual` input

      test "#3" do
        let input = { a0: 0, a1: 1, a2: 2 }
        build pickRecord input `shouldEqual` { a1: 1, a2: 2 }

    suite "Function" do
      test "#0" do
        let input = {}
        pickRecord input `shouldEqual` {}

--       test "#1" do
--         let input = { a0: 0 }
--         build pickRecord input `shouldEqual` {}

      test "#2" do
        let input = { a0: 0 }
        pickRecord input `shouldEqual` input
