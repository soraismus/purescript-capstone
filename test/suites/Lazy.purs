module Test.Suites.Lazy
  ( suites
  ) where

import Prelude

import Data.Argonaut.Decode.Record.Lazy (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe(Just))
import Test.Unit (TestSuite, suite, test)
import Test.Utils (assert, check, withErrorMsg)

suites :: TestSuite
suites =
  suite "Lazy" do

    test "#0" do
      let
        value0 = { a0: 0
                 , a1: 1
                 , a2: Just 2
                 , a3: Just "hello"
                 , a4: Just true
                 }
        value1 = {}
        getResult
          :: Either
              String
              (
                  {}
                ->
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Maybe Int
                  , a3 :: Maybe String
                  , a4 :: Maybe Boolean
                  }
              )
        getResult = decodeJson value1 (encodeJson value0)
      assert $ check getResult withErrorMsg \f -> f value1 == value0

    test "#1" do
      let
        value0 = { a0: 0
                 , a1: 1
                 , a2: Just 2
                 , a3: Just "hello"
                 , a4: Just true
                 }
        value1 = { b0: "b0" }
        getResult
          :: Either
              String
              (
                  { b0 :: String }
                ->
                  { a0 :: Int
                  , a1 :: Int
                  , a2 :: Maybe Int
                  , a3 :: Maybe String
                  , a4 :: Maybe Boolean
                  , b0 :: String
                  }
              )
        getResult = decodeJson value1 (encodeJson value0)
      assert
        $ check
            getResult
            withErrorMsg
            \f -> f value1 == { a0: value0.a0
                              , a1: value0.a1
                              , a2: value0.a2
                              , a3: value0.a3
                              , a4: value0.a4
                              , b0: value1.b0
                              }

--     test "#3 -- Does Not Compile" do
--       let
--         value0 = { a0: 0
--                  , a1: 1
--                  , a2: Just 2
--                  , a3: Just "hello"
--                  , a4: Just true
--                  }
--         value1 = { a0: 1000 }
--         getResult
--           :: Either
--               String
--               (
--                   { a0 :: Int }
--                 ->
--                   { a0 :: Int
--                   , a1 :: Int
--                   , a2 :: Maybe Int
--                   , a3 :: Maybe String
--                   , a4 :: Maybe Boolean
--                   }
--               )
--         getResult = decodeJson value0 (encodeJson value0)
--       assert
--         $ check
--             getResult
--             withErrorMsg
--             \f -> f value1 == { a0: value0.a0
--                               , a1: value0.a1
--                               , a2: value0.a2
--                               , a3: value0.a3
--                               , a4: value0.a4
--                               }

--     test "#3 -- Does Not Compile" do
--       let
--         value0 = { a0: 0
--                  , a1: 1
--                  , a2: Just 2
--                  , a3: Just "hello"
--                  , a4: Just true
--                  }
--         value1 = { a0: "b0" }
--         getResult
--           :: Either
--               String
--               (
--                   { a0 :: String }
--                 ->
--                   { a0 :: Int
--                   , a1 :: Int
--                   , a2 :: Maybe Int
--                   , a3 :: Maybe String
--                   , a4 :: Maybe Boolean
--                   , b0 :: String
--                   }
--               )
--         getResult = decodeJson value0 (encodeJson value0)
--       assert
--         $ check
--             getResult
--             withErrorMsg
--             \f -> f value1 == { a0: value0.a0
--                               , a1: value0.a1
--                               , a2: value0.a2
--                               , a3: value0.a3
--                               , a4: value0.a4
--                               }



--     test "#1" do
--       let
--         result
--           :: Either
--               String
--               { a0 :: Int
--               , a1 :: Int
--               , a2 :: Maybe Int
--               , a3 :: Maybe String
--               , a4 :: Maybe Boolean
--               }
--         result =
--           decodeJsonWith
--             { a2: \json (rest :: { a0 :: Int, a1 :: Int }) -> Right $ Just 1002
--             , a3: \json (rest :: { a0 :: Int, a1 :: Int }) -> Right $ Just "bye"
--             , a4: \json (rest :: { a0 :: Int, a1 :: Int }) -> Right $ Just false
--             }
--             (encodeJson { a0: 0
--                         , a1: 1
--                         , a2: Just 2
--                         , a3: Just "hello"
--                         , a4: Just true
--                         })
--       assert $ check result withErrorMsg
--         (_ == { a0: 0
--               , a1: 1
--               , a2: Just 1002
--               , a3: Just "bye"
--               , a4: Just false
--               })
--
--     test "#2" do
--       let
--         result
--           :: Either
--               String
--               { a0 :: Int
--               , a1 :: Int
--               , a2 :: Maybe Int
--               , a3 :: Maybe String
--               , a4 :: Maybe Boolean
--               }
--         result =
--           decodeJsonWith
--             { a0: \json (rest :: {}) -> Right 1000
--             , a1: \json (rest :: {}) -> Right 1001
--             , a2: \json (rest :: {}) -> Right $ Just 1002
--             , a3: \json (rest :: {}) -> Right $ Just "bye"
--             , a4: \json (rest :: {}) -> Right $ Just false
--             }
--             (encodeJson { a0: 0
--                         , a1: 1
--                         , a2: Just 2
--                         , a3: Just "hello"
--                         , a4: Just true
--                         })
--       assert $ check result withErrorMsg
--         (_ == { a0: 1000
--               , a1: 1001
--               , a2: Just 1002
--               , a3: Just "bye"
--               , a4: Just false
--               })
--
--     test "#3" do
--       let
--         result
--           :: Either
--               String
--               { a0 :: Int
--               , a1 :: Int
--               , a2 :: Maybe Int
--               , a3 :: Maybe String
--               , a4 :: Maybe Boolean
--               }
--         result =
--           decodeJsonWith
--             { a0: \json rest -> Right 1000
--             , a1: \json rest -> Right 1001
--             , a2: \json rest -> Right $ Just 1002
--             , a3: \json rest -> Right $ Just "bye"
--             , a4: \json rest -> Right $ Just false
--             }
--             (encodeJson { a0: 0
--                         , a1: 1
--                         , a2: Just 2
--                         , a3: Just "hello"
--                         , a4: Just true
--                         })
--       assert $ check result withErrorMsg
--         (_ == { a0: 1000
--               , a1: 1001
--               , a2: Just 1002
--               , a3: Just "bye"
--               , a4: Just false
--               })
--
--     test "#4" do
--       let
--         result
--           :: Either
--               String
--               { a0 :: Int
--               , a1 :: Int
--               , a2 :: Maybe Int
--               , a3 :: Maybe String
--               , a4 :: Maybe Boolean
--               }
--         result =
--           decodeJsonWith
--             { a2: \json (rest :: { a0 :: Int, a1 :: Int }) -> Right $ Just rest.a0
--             , a3: \json (rest :: { a0 :: Int, a1 :: Int }) -> Right $ Just "bye"
--             , a4: \json (rest :: { a0 :: Int, a1 :: Int }) -> Right $ Just false
--             }
--             (encodeJson { a0: 0
--                         , a1: 1
--                         , a2: Just 2
--                         , a3: Just "hello"
--                         , a4: Just true
--                         })
--       assert $ check result withErrorMsg
--         (_ == { a0: 0
--               , a1: 1
--               , a2: Just 0
--               , a3: Just "bye"
--               , a4: Just false
--               })
--     test "#5" do
--       let
--         result
--           :: Either
--               String
--               { a0 :: Int
--               , a1 :: Int
--               , a2 :: Maybe Int
--               , a3 :: Maybe String
--               , a4 :: Maybe Boolean
--               }
--         result =
--           decodeJsonWith
--             { a2: \json (rest :: { a0 :: Int, a1 :: Int }) -> Right $ Just rest.a0
--             , a3: \json rest -> Right $ Just $ show rest.a0
--             , a4: \json rest -> Right $ Just $ (rest.a1 `mod` 2 == 0)
--             }
--             (encodeJson { a0: 0
--                         , a1: 1
--                         , a2: Just 2
--                         , a3: Just "hello"
--                         , a4: Just true
--                         })
--       assert $ check result withErrorMsg
--         (_ == { a0: 0
--               , a1: 1
--               , a2: Just 0
--               , a3: Just $ show 0
--               , a4: Just (1 `mod` 2 == 0)
--                   })
--     test "#6" do
--       let
--         isEven :: Int -> Boolean
--         isEven i = (i `mod` 2) == 0
--         result
--           :: Either
--               String
--               { a0 :: Int
--               , a1 :: Int
--               , a2 :: Maybe Int
--               , a3 :: Maybe String
--               , a4 :: Maybe Boolean
--               }
--         result =
--           decodeJsonWith
--             { a4: \json rest -> Right $ isEven <$> rest.a2 }
--             (encodeJson { a0: 0
--                         , a1: 1
--                         , a2: Just 2
--                         , a3: Just "hello"
--                         , a4: Just true
--                         })
--       assert $ check result withErrorMsg
--         (_ == { a0: 0
--               , a1: 1
--               , a2: Just 2
--               , a3: Just "hello"
--               , a4: isEven <$> Just 2
--               })
