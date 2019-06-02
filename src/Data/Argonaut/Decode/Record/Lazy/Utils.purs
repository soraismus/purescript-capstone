module Data.Argonaut.Decode.Record.Lazy.Utils
  ( decodeJson
  , decodeJson'
  ) where

import Prelude (class Functor, map, (#))

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
   . D.GDecodeJson Function f Record l Nil () l r
  => Functor f
  => RowToList r l
  => RowToList r l
  => Status f
  => Json
  -> f (Record r)
decodeJson json = map ((#) {}) (decodeJson' json)

decodeJson'
  :: forall f g l0 l1 l2 r1 r2
   . D.GDecodeJson Function f g l0 l1 r1 l2 r2
  => RowToList r1 l1
  => RowToList r2 l2
  => Status f
  => Json
  -> f (g r1 -> g r2)
decodeJson' json = reportJson go json
  where
  go :: Object Json -> f (g r1 -> g r2)
  go object =
    D.gDecodeJson
      (RLProxy :: RLProxy l1)
      (RLProxy :: RLProxy l2)
      object
