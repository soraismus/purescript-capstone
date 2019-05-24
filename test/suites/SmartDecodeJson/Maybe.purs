module Test.Suites.SmartDecodeJson.Maybe
  ( suitex
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Smart (smartDecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just))
import Test.Unit (TestSuite, suite, test)
import Test.Utils (assertEquivalence)

newtype Id a = Id a
derive instance genericId :: Generic a x => Generic (Id a) _
derive instance eqId :: Eq a => Eq (Id a)
derive newtype instance decodeJsonId :: DecodeJson a => DecodeJson (Id a)
derive newtype instance encodeJsonId :: EncodeJson a => EncodeJson (Id a)
derive newtype instance showId :: Show a => Show (Id a)

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
    test "#7" do
      let value = { a0: Id 0 }
      let result = smartDecodeJson $ encodeJson value
      assertEquivalence result value

    test "#X" do
      let value =
          { a0: Id 0
          , a1: Just 1
          , a2: 2
          , a3: (Nothing :: Just Int)
          , a4: Id "4"
          , a5: "5"
          , a6: [6]
          , a7: ([] :: Array Boolean)
          , a8: true
          , a9: Just 9
          }
      let result = smartDecodeJson $ encodeJson value
      assertEquivalence result value
