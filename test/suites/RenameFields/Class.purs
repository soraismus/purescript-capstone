module Test.Suites.RenameFields.Class
  ( suitex
  ) where

import Prelude (discard)

-- import Data.Argonaut.Decode.Record.RenameFields (renameFields)
import Data.Argonaut.Decode.Record.RenameFields.Class (renameFields)
import Data.Either (Either(Left))
import Data.List (List, (:))
import Data.List (List(Nil)) as List
import Data.Maybe (Maybe(Just))
import Data.Symbol (SProxy(SProxy))
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (shouldEqual)
import Type.Row (Cons, Nil)

suitex :: TestSuite
suitex =
  suite "RenameFields" do
    test "#0" do
      let value = {}
      let result =
            renameFields
              (RLProxy :: RLProxy Nil)
              (RLProxy :: RLProxy Nil)
              (RLProxy :: RLProxy Nil)
              {}
              value
      result `shouldEqual` value

    test "#1" do
      let value = { a0: 0 }
      let result =
            renameFields
              (RLProxy :: RLProxy (Cons "a0" Int Nil))
              (RLProxy :: RLProxy (Cons "a0" (SProxy "b0") Nil))
              (RLProxy :: RLProxy (Cons "b0" Int Nil))
              { a0: (SProxy :: SProxy "b0") }
              value
      result `shouldEqual` { b0: 0 }

    test "#2" do
      let
        proxy0
          :: RLProxy
                (Cons "a0" Int
                (Cons "a1" String
                (Cons "a2" Boolean Nil)))
        proxy0 = RLProxy

        proxy1
          :: RLProxy
                (Cons "a0" (SProxy "b0")
                (Cons "a1" (SProxy "b1")
                (Cons "a2" (SProxy "b2") Nil)))
        proxy1 = RLProxy

        proxy2
          :: RLProxy
                (Cons "b0" Int
                (Cons "b1" String
                (Cons "b2" Boolean Nil)))
        proxy2 = RLProxy

        value = { a0: 0, a1: "1", a2: true }

        result =
          renameFields
            proxy0
            proxy1
            proxy2
            { a0: (SProxy :: SProxy "b0")
            , a1: (SProxy :: SProxy "b1")
            , a2: (SProxy :: SProxy "b2")
            }
            value
      result `shouldEqual` { b0: value.a0, b1: value.a1, b2: value.a2 }

    test "#3" do
      let
        proxy0
          :: RLProxy
                (Cons "a0" Int
                (Cons "a1" String
                (Cons "a2" Boolean
                (Cons "a3" (Maybe Int)
                (Cons "a4" (Either String Int)
                (Cons "a5" (Array Int)
                (Cons "a6" (List Int)
                (Cons "a7" { c0 :: Int, c1 :: String } Nil))))))))
        proxy0 = RLProxy

        proxy1
          :: RLProxy
                (Cons "a0" (SProxy "b0")
                (Cons "a1" (SProxy "b1")
                (Cons "a2" (SProxy "b2")
                (Cons "a3" (SProxy "b3")
                (Cons "a4" (SProxy "b4")
                (Cons "a5" (SProxy "b5")
                (Cons "a6" (SProxy "b6")
                (Cons "a7" (SProxy "b7") Nil))))))))
        proxy1 = RLProxy

        proxy2
          :: RLProxy
                (Cons "b0" Int
                (Cons "b1" String
                (Cons "b2" Boolean
                (Cons "b3" (Maybe Int)
                (Cons "b4" (Either String Int)
                (Cons "b5" (Array Int)
                (Cons "b6" (List Int)
                (Cons "b7" { c0 :: Int, c1 :: String } Nil))))))))
        proxy2 = RLProxy

        value =
          { a0: 0
          , a1: "1"
          , a2: true
          , a3: Just 3
          , a4: Left "4"
          , a5: [5]
          , a6: 6 : List.Nil
          , a7: { c0: 70, c1: "71" }
          }

        result =
          renameFields
            proxy0
            proxy1
            proxy2
            { a0: (SProxy :: SProxy "b0")
            , a1: (SProxy :: SProxy "b1")
            , a2: (SProxy :: SProxy "b2")
            , a3: (SProxy :: SProxy "b3")
            , a4: (SProxy :: SProxy "b4")
            , a5: (SProxy :: SProxy "b5")
            , a6: (SProxy :: SProxy "b6")
            , a7: (SProxy :: SProxy "b7")
            }
            value
      result
        `shouldEqual`
        { b0: value.a0
        , b1: value.a1
        , b2: value.a2
        , b3: value.a3
        , b4: value.a4
        , b5: value.a5
        , b6: value.a6
        , b7: value.a7
        }
