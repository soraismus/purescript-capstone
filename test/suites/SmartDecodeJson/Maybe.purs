module Test.Suites.SmartDecodeJson.Maybe
  ( suitex
  ) where

import Effect.Console (log)
import Effect.Class (liftEffect)

import Prelude
import Data.Argonaut.Core (isNull, jsonNull, stringify)

import Control.MonadZero (empty)
import Control.Plus (class Plus)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Smart (smartDecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Maybe.First (First(First))
import Data.String.CodePoints (CodePoint, codePointFromChar)
import Data.Tuple (Tuple(Tuple))
import Test.Unit (TestSuite, suite, test)
import Test.Utils (assertEquivalence)

newtype Id a = Id a
derive instance genericId :: Generic a x => Generic (Id a) _
derive instance eqId :: Eq a => Eq (Id a)
derive newtype instance decodeJsonId :: DecodeJson a => DecodeJson (Id a)
derive newtype instance encodeJsonId :: EncodeJson a => EncodeJson (Id a)
derive newtype instance showId :: Show a => Show (Id a)

newtype First' a = First' (First a)
derive instance genericFirst' :: Generic a x => Generic (First' a) _
derive instance eqFirst' :: Eq a => Eq (First' a)
derive newtype instance showFirst' :: Show a => Show (First' a)
derive newtype instance plusFirst' :: Plus First'
instance decodeJsonFirst' :: DecodeJson a => DecodeJson (First' a) where
  decodeJson j
    | isNull j = pure $ First' $ First Nothing
    | otherwise = (First' <<< First <<< Just) <$> decodeJson j
instance encodeJsonFirst' :: EncodeJson a => EncodeJson (First' a) where
  encodeJson (First' (First Nothing)) = jsonNull
  encodeJson (First' (First (Just a))) = encodeJson a

suitex :: TestSuite
suitex =
  suite "Maybe" do
    suite "Maybe" do
      test "Just 0" do
        let value = Just 0
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
      test "Nothing :: Maybe Int" do
        let value = Nothing :: Maybe Int
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
    suite "Tuple" do
      test "Tuple 0 \"hello\"" do
        let value = Tuple 0 "hello"
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
    suite "Either" do
      test "Left 0 :: Either Int String" do
        let value = Left 0 :: Either Int String
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
      test "Right \"hello\" :: Either Int String" do
        let value = Right "hello" :: Either Int String
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
    suite "Unit" do
      test "Unit" do
        let value = unit
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
    suite "Boolean" do
      test "true" do
        let value = true
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
      test "false" do
        let value = false
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
    suite "Number" do
      test "0.35" do
        let value = 0.35 :: Number
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
    suite "Int" do
      test "1" do
        let value = 1 :: Int
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
    suite "String" do
      test "\"hello\"" do
        let value = "hello"
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
--   suite "Json" do
--   suite "NonEmptyArray" do
--   suite "NonEmptyList" do
--   suite "ForeignObject" do
    suite "CodePoint" do
      test "codePointFromChar 'a'" do
        let value = codePointFromChar 'a'
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
    suite "Array" do
      test "[] :: Array Int" do
        let value = [] :: Array Int
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
      test "[0]" do
        let value = [0]
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
      test "[0, 1, 2, 3]" do
        let value = [0, 1, 2, 3]
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
      test "[[], [0], [1, 2]]" do
        let value = [[], [0], [1, 2]]
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
      test "[[[]], [[0], [1, 2]]]" do
        let value = [[[]], [[0], [1, 2]]]
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
    suite "List" do
      test "Nil :: List Int" do
        let value = Nil :: List Int
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
      test "0 : Nil" do
        let value = 0 : Nil
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
      test "0 : 1 : 2 : Nil" do
        let value = 0 : 1 : 2 : 3 : Nil
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value
      test "((Nil) : (0 : Nil) : (1 : 2 : Nil) : Nil)" do
        let value = ((Nil) : (0 : Nil) : (1 : 2 : Nil) : Nil)
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value

    suite "Record -- no absent fields" do
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
                , a3: (Nothing :: Maybe Int)
                , a4: Id "4"
                , a5: "5"
                , a6: [6]
                , a7: ([] :: Array Boolean)
                , a8: true
                , a9: Just 9
                }
        let result = smartDecodeJson $ encodeJson value
        assertEquivalence result value

    suite "Record -- with absent fields" do
      test "#0" do
        let
          result :: Either String { a0 :: Maybe Int }
          result = smartDecodeJson $ encodeJson {}
        assertEquivalence result { a0: empty }
      test "#1" do
        let
          result :: Either String { a0 :: Array Int }
          result = smartDecodeJson $ encodeJson {}
        assertEquivalence result { a0: empty }
      test "#2" do
        let
          result :: Either String { a0 :: List Int }
          result = smartDecodeJson $ encodeJson {}
        assertEquivalence result { a0: empty }
      test "#3" do
        let
          result :: Either String { a0 :: First' Int }
          result = smartDecodeJson $ encodeJson {}
        assertEquivalence result { a0: empty }
      test "#4" do
        let
          result
            :: Either
                  String
                  { a0 :: Maybe Int
                  , a1 :: Array Int
                  , a2 :: List Int
                  , a3 :: First' Int
                  }
          result = smartDecodeJson $ encodeJson {}
        assertEquivalence
          result
          { a0: empty
          , a1: empty
          , a2: empty
          , a3: empty
          }
      test "#5" do
        let
          result
            :: Either
                  String
                  { a00 :: Maybe String
                  , a01 :: Maybe String
                  , a02 :: Id String
                  , a03 :: String
                  , a04 :: Id Int
                  , a05 :: Array Int
                  , a06 :: Array Int
                  , a07 :: Int
                  , a08 :: Boolean
                  , a09 :: List Boolean
                  , a10 :: List Boolean
                  , a11 :: Id Boolean
                  , a12 :: Array (List Int)
                  , a13 :: Id (Array (List Int))
                  , a14 :: First' (Array (List Int))
                  , a15 :: First' (Array (List Int))
                  }
          result =
            smartDecodeJson
              $ encodeJson
                  { a01: Nothing :: Maybe String
                  , a02: Id "2"
                  , a03: "3"
                  , a04: Id 4
                  , a05: [] :: Array Int
                  , a07: 7
                  , a08: true
                  , a10: Nil :: List Boolean
                  , a11: Id true
                  , a12: [Nil, (0 : Nil), (0 : 1 : Nil)]
                  , a13: Id [Nil, (0 : Nil), (0 : 1 : Nil)]
                  , a14: empty :: First' (Array (List Int))
                  }
        assertEquivalence
          result
          { a00: empty
          , a01: Nothing
          , a02: Id "2"
          , a03: "3"
          , a04: Id 4
          , a05: []
          , a06: empty
          , a07: 7
          , a08: true
          , a09: empty
          , a10: Nil
          , a11: Id true
          , a12: [Nil, (0 : Nil), (0 : 1 : Nil)]
          , a13: Id [Nil, (0 : Nil), (0 : 1 : Nil)]
          , a14: empty
          , a15: empty
          }

    suite "Record -- with records having absent fields" do
      test "#0" do
        let x = encodeJson { a0: {} }
        liftEffect $ log $ stringify x
        let
          result :: Either String { a0 :: { b0 :: Maybe Int } }
          result = smartDecodeJson $ encodeJson { a0: {} }
        liftEffect $ log $ show result
        assertEquivalence result { a0: { b0: Nothing } }
