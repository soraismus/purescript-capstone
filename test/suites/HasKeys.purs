module Test.Suites.HasKeys
  ( suites
  ) where

import Prelude (Unit, discard)

import Record.Extra.HasKeys (class HasKeys)
import Record.Extra.SameSize (class SameSize)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row (class RowToList, Cons, Nil, RProxy(RProxy), kind RowList)

assertHasKeys :: forall l r. HasKeys l r => RLProxy l -> RProxy r -> Test
assertHasKeys _ _ = assert "Failure to have same keys" true

assertHasKeysAndSameSize
  :: forall l0 l1 r1
   . RowToList r1 l1
  => HasKeys l0 r1
  => SameSize l0 l1
  => RLProxy l0
  -> RProxy r1
  -> Test
assertHasKeysAndSameSize _ _ =
  assert "Failure to have same keys or same size" true

suites :: TestSuite
suites =
  suite "HasKeys" do

    suite "the two arguments have identical size" do
      test "Empty rowlist" do
        let value0 = RLProxy :: RLProxy Nil
        let value1 = RProxy :: RProxy ()
        assertHasKeysAndSameSize value0 value1
      test "Single key" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let value1 = RProxy :: RProxy (a0 :: Int)
        assertHasKeysAndSameSize value0 value1
      test "Single key different values" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let value1 = RProxy :: RProxy (a0 :: String)
        assertHasKeysAndSameSize value0 value1
      test "Two keys" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
        assertHasKeysAndSameSize value0 value1
      test "Two keys with different values" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Unit Nil))
        let value1 = RProxy :: RProxy (a0 :: String, a1 :: Int)
        assertHasKeysAndSameSize value0 value1
      test "Three keys" do
        let
          value0 :: RLProxy (Cons "a0" Int (Cons "a1" Int (Cons "a2" Int Nil)))
          value0 = RLProxy
          value1 :: RProxy (a0 :: Int, a1 :: Int, a2 :: Int)
          value1 = RProxy
        assertHasKeysAndSameSize value0 value1
      test "Three keys with different values" do
        let
          value0 :: RLProxy (Cons "a0" Int (Cons "a1" Unit (Cons "a2" Int Nil)))
          value0 = RLProxy
          value1 :: RProxy (a0 :: String, a1 :: Int, a2 :: Boolean)
          value1 = RProxy
        assertHasKeysAndSameSize value0 value1
      test "Multiple keys" do
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
        assertHasKeysAndSameSize value0 value1
      test "Multiple keys with different values" do
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
                  ( a0 :: String
                  , a1 :: String
                  , a2 :: String
                  , a3 :: String
                  , a4 :: String
                  , a5 :: String
                  , a6 :: String
                  )
            value1 = RProxy
        assertHasKeysAndSameSize value0 value1

    suite "the first argument has smaller size" do
      test "Empty rowlist #0" do
        let value0 = RLProxy :: RLProxy Nil
        let value1 = RProxy :: RProxy ()
        assertHasKeys value0 value1
      test "Empty rowlist #1" do
        let value0 = RLProxy :: RLProxy Nil
        let value1 = RProxy :: RProxy (a0 :: Int)
        assertHasKeys value0 value1
      test "Empty rowlist #2" do
        let value0 = RLProxy :: RLProxy Nil
        let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int, a2 :: Int, a3 :: Int)
        assertHasKeys value0 value1
      test "Single key #0" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int)
        assertHasKeys value0 value1
      test "Single key #1" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let value1 = RProxy :: RProxy (a1 :: Int, a0 :: Int)
        assertHasKeys value0 value1
      test "Single key #2" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int, a2 :: Int, a3 :: Int)
        assertHasKeys value0 value1
      test "Single key #3" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let value1 = RProxy :: RProxy (a1 :: Int, a2 :: Int, a0 :: Int, a3 :: Int)
        assertHasKeys value0 value1
      test "Two keys #0" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let value1 = RProxy :: RProxy (a0 :: Int, a1 :: Int, a2 :: Int)
        assertHasKeys value0 value1
      test "Two keys #0" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let value1 = RProxy :: RProxy (a2 :: Int, a1 :: Int, a0 :: Int)
        assertHasKeys value0 value1
      test "Two keys with different values" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Unit Nil))
        let value1 = RProxy :: RProxy (a2 :: Int, a1 :: Int, a0 :: String)
        assertHasKeys value0 value1
      test "Multiple keys with different values" do
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
                  , a0 :: String
                  , b1 :: String
                  , a1 :: String
                  , b2 :: String
                  , a2 :: String
                  , b3 :: String
                  , a3 :: String
                  , b4 :: String
                  , a4 :: String
                  , b5 :: String
                  , a5 :: String
                  , b6 :: String
                  , a6 :: String
                  )
            value1 = RProxy
        assertHasKeys value0 value1
