module Data.Argonaut.Decode.Record.Lazy.Utils
  ( decodeJson
  , decodeJson'
  ) where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Record.Lazy.Class
  ( class GDecodeJson
  , gDecodeJson
  ) as D
import Data.Argonaut.Decode.Record.Utils (reportJson)
import Data.Status (class Status)
import Foreign.Object (Object)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row (class RowToList, Nil)

decodeJson
  :: forall f l r
   . D.GDecodeJson f Record l Nil () l r
  => RowToList r l
  => RowToList r l
  => Status f
  => Json
  -> f (Record r)
decodeJson json = decodeJson' json {}

decodeJson'
  :: forall f g l0 l1 l2 r1 r2
   . D.GDecodeJson f g l0 l1 r1 l2 r2
  => RowToList r1 l1
  => RowToList r2 l2
  => Status f
  => Json
  -> g r1
  -> f (g r2)
decodeJson' json record = reportJson go json
  where
  go :: Object Json -> f (g r2)
  go object =
    D.gDecodeJson
      (RLProxy :: RLProxy l1)
      (RLProxy :: RLProxy l2)
      object
      record
