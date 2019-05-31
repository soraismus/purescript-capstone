module Data.Argonaut.Decode.Record.Override.Utils
  ( decodeJsonWith
  ) where

import Prelude (class Bind, bind, ($))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class GDecodeJson)
import Data.Argonaut.Decode.Record.Override.Class
  ( class DecodeJsonWith
  , decodeJsonWith
  ) as D
import Data.Argonaut.Decode.Record.Utils (reportJson, reportObject)
import Data.Status (class Status, report)
import Foreign.Object (Object)
import Record (disjointUnion)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row (class Nub, class RowToList, class Union)

decodeJsonWith
  :: forall dr dl f l0 l1 l2 r0 r1 r2
   . Bind f
  => D.DecodeJsonWith f dl dr l0 r0
  => GDecodeJson r1 l1
  => Nub r2 r2
  => RowToList r1 l1
  => RowToList r2 l2
  => RowToList dr dl
  => Status f
  => Union r0 r1 r2
  => Record dr
  -> Json
  -> f (Record r2)
decodeJsonWith decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record r2)
  go object = do
    record0 <-
      D.decodeJsonWith
        (RLProxy :: RLProxy dl)
        decoderRecord
        object
    record1 <- reportObject object (RLProxy :: RLProxy l1)
    report $ disjointUnion record0 record1
