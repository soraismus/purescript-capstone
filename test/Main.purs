module Test.Main
  ( main
  ) where

import Prelude (Unit, discard)

import Effect (Effect)
import Test.Suites.Cross (suites) as Cross
import Test.Suites.HasKeys (suites) as HasKeys
import Test.Suites.Lazy (suites) as Lazy
import Test.Suites.MapRecord (suites) as MapRecord
import Test.Suites.Override (suites) as Override
import Test.Suites.PickRecord (suites) as PickRecord
import Test.Suites.Record.Extra.RenameFields (suites) as RenameFields
import Test.Suites.SameSize (suites) as SameSize
import Test.Suites.SubFields (suites) as SubFields
import Test.Suites.Tolerant (suites) as Tolerant
import Test.Suites.Record.Extra.Utils.Singleton as Singleton
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  Cross.suites
  HasKeys.suites
  Lazy.suites
  MapRecord.suites
  PickRecord.suites
  Override.suites
  RenameFields.suites
  SameSize.suites
  Singleton.suites
  SubFields.suites
  Tolerant.suites
