module Test.Suites.SameSize
  ( suites
  ) where

import Prelude (discard, pure, unit, ($))

import Record.Extra.SameSize (class SameSize)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row (Cons, Nil)

assertSameSize
  :: forall l0 l1
   . SameSize l0 l1
  => RLProxy l0
  -> RLProxy l1
  -> Test
assertSameSize _ _ = assert "Failure to have same size" true

suites :: TestSuite
suites =
  suite "SameSize" do
    suite "SameSize constraint is satisfied" do
      test "No key" do
        let value0 = RLProxy :: RLProxy Nil
        let value1 = RLProxy :: RLProxy Nil
        assertSameSize value0 value1
      test "One key #0" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let value1 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        assertSameSize value0 value1
      test "One key #1" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let value1 = RLProxy :: RLProxy (Cons "a0" String Nil)
        assertSameSize value0 value1
      test "One key #2" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let value1 = RLProxy :: RLProxy (Cons "b0" Int Nil)
        assertSameSize value0 value1
      test "One key #2" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int Nil)
        let value1 = RLProxy :: RLProxy (Cons "b0" String Nil)
        assertSameSize value0 value1
      test "Two keys #0" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let value1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        assertSameSize value0 value1
      test "Two keys #1" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let value1 = RLProxy :: RLProxy (Cons "a0" String (Cons "a1" Int Nil))
        assertSameSize value0 value1
      test "Two keys #2" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let value1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" String Nil))
        assertSameSize value0 value1
      test "Two keys #3" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let value1 = RLProxy :: RLProxy (Cons "a0" String (Cons "a1" String Nil))
        assertSameSize value0 value1
      test "Two keys #4" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let value1 = RLProxy :: RLProxy (Cons "b0" Int (Cons "a1" Int Nil))
        assertSameSize value0 value1
      test "Two keys #5" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let value1 = RLProxy :: RLProxy (Cons "a0" Int (Cons "b1" Int Nil))
        assertSameSize value0 value1
      test "Two keys #6" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let value1 = RLProxy :: RLProxy (Cons "b0" Int (Cons "b1" Int Nil))
        assertSameSize value0 value1
      test "Two keys #7" do
        let value0 = RLProxy :: RLProxy (Cons "a0" Int (Cons "a1" Int Nil))
        let value1 = RLProxy :: RLProxy (Cons "b0" String (Cons "b1" String Nil))
        assertSameSize value0 value1
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
              :: RLProxy
                  (Cons "b0" String (
                  (Cons "b1" String (
                  (Cons "b2" String (
                  (Cons "b3" String (
                  (Cons "b4" String (
                  (Cons "b5" String (
                  (Cons "b6" String Nil)))))))))))))
            value1 = RLProxy
        assertSameSize value0 value1
    suite "SameSize constraint is not satisfied -- CANNOT COMPILE" do
      test "Mismatched size #0 -- Does Not Compile" $ pure unit
  --       let
  --           value0 :: RLProxy Nil
  --           value0 = RLProxy
  --           value1
  --             :: RLProxy
  --                 (Cons "b0" String (
  --                 (Cons "b1" String (
  --                 (Cons "b2" String Nil)))))
  --           value1 = RLProxy
  --       assertSameSize value0 value1
      test "Mismatched size #1 -- Does Not Compile" $ pure unit
  --       let
  --           value0
  --             :: RLProxy
  --                 (Cons "a0" String (
  --                 (Cons "a1" String (
  --                 (Cons "a2" String Nil)))))
  --           value0 = RLProxy
  --           value1 :: RLProxy Nil
  --           value1 = RLProxy
  --       assertSameSize value0 value1
      test "Mismatched size #2 -- Does Not Compile" $ pure unit
  --       let
  --           value0
  --             :: RLProxy
  --                 (Cons "a0" Int (
  --                 (Cons "a1" Int Nil)))
  --           value0 = RLProxy
  --           value1
  --             :: RLProxy
  --                 (Cons "b0" String (
  --                 (Cons "b1" String (
  --                 (Cons "b2" String Nil)))))
  --           value1 = RLProxy
  --       assertSameSize value0 value1
      test "Mismatched size #3 -- Does Not Compile" $ pure unit
  --       let
  --           value0
  --             :: RLProxy
  --                 (Cons "a0" String (
  --                 (Cons "a1" String (
  --                 (Cons "a2" String Nil)))))
  --           value0 = RLProxy
  --           value1
  --             :: RLProxy
  --                 (Cons "b0" Int (
  --                 (Cons "b1" Int Nil)))
  --           value1 = RLProxy
  --       assertSameSize value0 value1
