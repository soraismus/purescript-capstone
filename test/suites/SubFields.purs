module Test.Suites.SubFields
  ( suites
  ) where

import Prelude (discard, pure, unit, ($))

import Data.SubFields (class SubFields)
import Data.SameSize (class SameSize)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row (class RowToList, Cons, Nil, RProxy(RProxy), kind RowList)

assertSubFields :: forall l r. SubFields l r => RLProxy l -> RProxy r -> Test
assertSubFields _ _ = assert "Failure to be a subrecord" true

assertSubFieldsAndSameSize
  :: forall l0 l1 r1
   . RowToList r1 l1
  => SubFields l0 r1
  => SameSize l0 l1
  => RLProxy l0
  -> RProxy r1
  -> Test
assertSubFieldsAndSameSize _ _ =
  assert "Failure to be an equivalent record." true

suites :: TestSuite
suites =
  suite "SubFields" do
    suite "SubFields constraint is satisfied" do
      suite "the two arguments have identical size" do
        test "Empty rowlist" do
          let value0 = RLProxy :: RLProxy Nil
          let value1 = RProxy :: RProxy ()
          assertSubFieldsAndSameSize value0 value1
        test "One field" do
          let value0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
          let value1 = RProxy :: RProxy (a0 :: Int)
          assertSubFieldsAndSameSize value0 value1
        test "Two fields" do
          let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
          let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
          assertSubFieldsAndSameSize value0 value1
        test "Three fields" do
          let
            value0 :: RLProxy (Cons "a0" Int (Cons "a1" Int (Cons "a2" Int Nil)))
            value0 = RLProxy
            value1 :: RProxy (a0 :: Int, a1 :: Int, a2 :: Int)
            value1 = RProxy
          assertSubFieldsAndSameSize value0 value1
        test "Multiple fields" do
          let
              value0
                :: RLProxy
                    (Cons "a0" Int (
                    (Cons "a1" Int (
                    (Cons "a2" Int (
                    (Cons "a3" Int (
                    (Cons "a4" Int (
                    (Cons "a5" Int (
                    (Cons "a6" Int Nil)))))))))))))
              value0 = RLProxy
              value1
                :: RProxy
                    ( a0 :: Int
                    , a1 :: Int
                    , a2 :: Int
                    , a3 :: Int
                    , a4 :: Int
                    , a5 :: Int
                    , a6 :: Int
                    )
              value1 = RProxy
          assertSubFieldsAndSameSize value0 value1
      suite "the first argument has smaller size" do
        test "Empty rowlist #0" do
          let value0 = RLProxy :: RLProxy Nil
          let value1 = RProxy :: RProxy ()
          assertSubFields value0 value1
        test "Empty rowlist #1" do
          let value0 = RLProxy :: RLProxy Nil
          let value1 = RProxy :: RProxy (a0 :: Int)
          assertSubFields value0 value1
        test "Empty rowlist #2" do
          let value0 = RLProxy :: RLProxy Nil
          let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int, a2 :: Int, a3 :: Int)
          assertSubFields value0 value1
        test "One field #0" do
          let value0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
          let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
          assertSubFields value0 value1
        test "One field #1" do
          let value0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
          let value1 = RProxy :: RProxy (a1 :: Int, a0 :: Int)
          assertSubFields value0 value1
        test "One field #2" do
          let value0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
          let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int, a2 :: Int, a3 :: Int)
          assertSubFields value0 value1
        test "One field #3" do
          let value0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
          let value1 = RProxy :: RProxy (a1 :: Int, a2 :: Int, a0 :: Int, a3 :: Int)
          assertSubFields value0 value1
        test "Two fields #0" do
          let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
          let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int, a2 :: Int)
          assertSubFields value0 value1
        test "Two fields #0" do
          let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
          let value1 = RProxy :: RProxy (a2 :: Int, a1 :: Int, a0 :: Int)
          assertSubFields value0 value1
        test "Multiple fields" do
          let
              value0
                :: RLProxy
                    (Cons "a0" Int (
                    (Cons "a1" Int (
                    (Cons "a2" Int (
                    (Cons "a3" Int (
                    (Cons "a4" Int (
                    (Cons "a5" Int (
                    (Cons "a6" Int Nil)))))))))))))
              value0 = RLProxy
              value1
                :: RProxy
                    ( b0 :: String
                    , a6 :: Int
                    , b1 :: Int
                    , a1 :: Int
                    , b2 :: Boolean
                    , a4 :: Int
                    , b3 :: String
                    , a3 :: Int
                    , b4 :: Int
                    , a2 :: Int
                    , b5 :: Boolean
                    , a5 :: Int
                    , b6 :: String
                    , a0 :: Int
                    )
              value1 = RProxy
          assertSubFields value0 value1
    suite "SubFields constraint is not satisfied -- CANNOT COMPILE" do
      test "Single field with different keys -- Does Not Compile" $ pure unit
--         let value0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
--         let value1 = RProxy :: RProxy (b0 :: Int)
--         assertSubFields value0 value1
      test "Single field with different values -- Does Not Compile" $ pure unit
--         let value0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
--         let value1 = RProxy :: RProxy (a0 :: String)
--         assertSubFields value0 value1
      test "Two fields with different keys -- Does Not Compile" $ pure unit
--         let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
--         let value1 = RProxy :: RProxy (b0 :: Int, b1 :: Int)
--         assertSubFields value0 value1
      test "Two fields with different values -- Does Not Compile" $ pure unit
--         let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
--         let value1 = RProxy :: RProxy (a0 :: String, a1 :: Int)
--         assertSubFields value0 value1
      test "Multiple fields with different keys and values -- Does Not Compile" $ pure unit
--         let
--             value0
--               :: RLProxy
--                   (Cons "a0" Int (
--                   (Cons "a1" Int (
--                   (Cons "a2" Int (
--                   (Cons "a3" Int (
--                   (Cons "a4" Int (
--                   (Cons "a5" Int (
--                   (Cons "a6" Int Nil)))))))))))))
--             value0 = RLProxy
--             value1
--               :: RProxy
--                   ( b0 :: String
--                   , b1 :: String
--                   , b2 :: String
--                   , b3 :: String
--                   , b4 :: String
--                   , b5 :: String
--                   , b6 :: String
--                   )
--             value1 = RProxy
--         assertSubFields value0 value1
