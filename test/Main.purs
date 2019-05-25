module Test.Main
  ( main
  ) where

import Prelude (Unit)

import Effect (Effect)
import Test.Suites.DecodeJson (suitex) as DecodeJson
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  DecodeJson.suitex
