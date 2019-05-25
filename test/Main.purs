module Test.Main
  ( main
  ) where

import Prelude (Unit)

import Effect (Effect)
import Test.Suites.SmartDecodeJson (suitex) as SmartDecodeJson
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  SmartDecodeJson.suitex
