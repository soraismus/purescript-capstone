module Test.Suites.MapRecord
  ( suites
  ) where

import Prelude

import Record.Builder (Builder)
import Record.Builder (insert, rename) as Builder
import Record.Extra.GMapRecord (mapRecord)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)

suites :: TestSuite
suites =
  suite "MapRecord" do
    test "#0" do
      shouldEqual
        { a0: 1 }
        $ mapRecord
            { a0: \i -> i + 1 }
            { a0: 0 }

    test "#1" do
      shouldEqual
        { a0: 1, a1: 2 }
        $ mapRecord
            { a0: \i -> i + 1
            , a1: \i -> i * 2
            }
            { a0: 0
            , a1: 1
            }

    test "#2" do
      shouldEqual
        { a0: 1, a1: 2, a2: 2 }
        $ mapRecord
            { a0: \i -> i + 1
            , a1: \i -> i * 2
            }
            { a0: 0
            , a1: 1
            , a2: 2
            }
