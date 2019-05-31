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
import Type.Row (class Nub, class RowToList, class Union)

decodeJsonWith
  :: forall dr dl f l1 l2 r0 r1 r2
   . Bind f
  => GDecodeJson r1 l1
  => Nub r2 r2
  => RowToList r1 l1
  => RowToList r2 l2
  => RowToList dr dl
  => Status f
  => Union r0 r1 r2
  => D.DecodeJsonWith f dl dr (Record r1)        l1 r1 r2           r0
  => Record dr
  -> Json
  -> f (Record r2)
decodeJsonWith decoderRecord = reportJson go
  where
  go :: Object Json -> f (Record r2)
  go object = do
    record1 <- reportObject object
    getFields0 <-
      D.decodeJsonWith
        (RLProxy :: RLProxy dl)
        (RLProxy :: RLProxy l1)
        decoderRecord
        object
        record1
    report $ getFields0 record1
