module Data.Argonaut.Decode.Record.Cross.Utils
  ( decodeJsonWith
  ) where

import Prelude (class Bind, bind, ($))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class GDecodeJson)
import Data.Argonaut.Decode.Record.Cross.Class
  ( class DecodeJsonWith
  , decodeJsonWith
  ) as D
import Data.Argonaut.Decode.Record.Utils (reportJson, reportObject)
import Data.Status (class Status, report)
import Foreign.Object (Object)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row (class Nub, class RowToList)

decodeJsonWith
  :: forall f l0 l2 r0 r1 r2 r3
   . Bind f
  => D.DecodeJsonWith f l0 r0 r1 l2 r2 r3 (Record r2)
  => GDecodeJson r2 l2
  => Nub r3 r3
  => RowToList r0 l0
  => RowToList r2 l2
  => Status f
  => Record r0
  -> Json
  -> f (Record r3)
decodeJsonWith decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record r3)
  go object = do
    (record2 :: Record r2) <- reportObject object
    (addFields1 :: Record r2 -> Record r3) <-
      D.decodeJsonWith
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l2)
        decoderRecord
        object
        record2
    report $ addFields1 record2
