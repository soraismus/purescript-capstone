module Test.Suites.SmartDecodeJson.Maybe
  ( suitex
  ) where

import Prelude

import Data.Argonaut.Decode.Smart (smartDecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Test.Unit (TestSuite, suite, test)
import Test.Utils (assertEquivalence)


suitex :: TestSuite
suitex =
  suite "Maybe" do
    test "{ a0: 1 }" do
      let value = { a0: 100 }
      let result = smartDecodeJson $ encodeJson value
      assertEquivalence result value
