module Test.Suites.DecodeJson
  ( suitex
  ) where

import Test.Suites.DecodeJson.Maybe (suitex) as Maybe
import Test.Unit (TestSuite, suite)

suitex :: TestSuite
suitex =
  suite "decodeJson" do
    Maybe.suitex
