module Test.Main
  ( main
  ) where

import Prelude (Unit, discard)

import Effect (Effect)
import Test.Suites.Cross (suites) as Cross
import Test.Suites.Lazy (suites) as Lazy
import Test.Suites.Override (suites) as Override
import Test.Suites.RenameFields.Class (suites) as RenameFields
import Test.Suites.SameKeys (suites) as SameKeys
import Test.Suites.SameSize (suites) as SameSize
import Test.Suites.SubFields (suites) as SubFields
import Test.Suites.Tolerant (suites) as Tolerant
import Test.Suites.Utils.Singleton (suites) as Singleton
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  Cross.suites
  Lazy.suites
  Override.suites
  RenameFields.suites
  SameKeys.suites
  SameSize.suites
  Singleton.suites
  SubFields.suites
  Tolerant.suites
