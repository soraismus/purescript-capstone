module Test.Main
  ( main
  ) where

import Prelude (Unit, discard)

import Effect (Effect)
import Test.Suites.Cross (suitex) as Cross
import Test.Suites.Tolerant (suitex) as Tolerant
import Test.Suites.Override (suitex) as Override
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  Cross.suitex
  Override.suitex
  Tolerant.suitex
