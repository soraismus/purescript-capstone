module Test.Suites.Utils.Singleton
  ( suites
  ) where

import Data.Argonaut.Decode.Record.Utils (singleton)
import Data.Symbol (SProxy(SProxy))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)

import Prelude (discard)

suites :: TestSuite
suites =
  suite "Singleton" do
    test "#0" do
      singleton (SProxy :: SProxy "a0") 0 `shouldEqual` { a0: 0 }
    test "#1" do
      let sproxy = SProxy :: SProxy " a . 1 "
      let value = [0, 1, 2, 3]
      singleton sproxy value `shouldEqual` { " a . 1 ": value }
