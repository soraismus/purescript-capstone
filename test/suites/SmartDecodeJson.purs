module Test.Suites.SmartDecodeJson
  ( suitex
  ) where

import Test.Suites.SmartDecodeJson.Maybe (suitex) as Maybe
import Test.Unit (TestSuite, suite)

suitex :: TestSuite
suitex =
  suite "smartDecodeJson" do
    Maybe.suitex
