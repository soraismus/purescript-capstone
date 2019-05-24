module Test.Suites.SmartDecodeJson.Maybe
  ( suitex
  ) where

import Prelude

import Data.Argonaut.Decode.Smart (smartDecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe(Just))
import Test.Unit (TestSuite, suite, test)
import Test.Utils (assertEquivalence)


suitex :: TestSuite
suitex =
  suite "Maybe" do
    test "#0" do
      let value = {}
      let result = smartDecodeJson $ encodeJson value
      assertEquivalence result value
    test "#1" do
      let value = { a0: 0 }
      let result = smartDecodeJson $ encodeJson value
      assertEquivalence result value
    test "#2" do
      let value = { a0: Just 0 }
      let result = smartDecodeJson $ encodeJson value
      assertEquivalence result value
    test "#3" do
      let value = { a0: 0, a1: Just 1 }
      let result = smartDecodeJson $ encodeJson value
      assertEquivalence result value
    test "#4" do
      let value = { a0: Just 0, a1: 1 }
      let result = smartDecodeJson $ encodeJson value
      assertEquivalence result value
    test "#5" do
      let value = { a0: 0, a1: Just 1, a2: "2" }
      let result = smartDecodeJson $ encodeJson value
      assertEquivalence result value
    test "#6" do
      let value = { a0: 0, a1: Just 1, a2: "2", a3: Just "3" }
      let result = smartDecodeJson $ encodeJson value
      assertEquivalence result value
