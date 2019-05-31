module Test.Main
  ( main
  ) where

import Prelude (Unit, discard)

import Effect (Effect)
import Test.Suites.Cross (suitex) as Cross
import Test.Suites.Override (suitex) as Override

import Test.Suites.RenameFields.Class (suitex) as RenameFields

import Test.Suites.SameKeys (suitex) as SameKeys
import Test.Suites.SameSize (suitex) as SameSize
import Test.Suites.SubFields (suitex) as SubFields
import Test.Suites.Tolerant (suitex) as Tolerant
import Test.Suites.Utils.Singleton (suites) as Singleton
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  Cross.suitex
  Override.suitex
  RenameFields.suitex
  SameKeys.suitex
  SameSize.suitex
  Singleton.suites
  SubFields.suitex
  Tolerant.suitex
