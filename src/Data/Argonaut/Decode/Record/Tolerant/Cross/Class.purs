module Data.Argonaut.Decode.Record.Tolerant.Cross.Class
  ( class DecodeJson
  , decodeJson
  ) where

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Record.Tolerant.Class
  ( class DecodeJson
  , class GDecodeJson
  , decodeJson
  , gDecodeJson
  ) as D
import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status (reportError)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row (class RowToList, Nil, kind RowList)

class DecodeJson a where
  decodeJson :: Json -> Either String a

instance decodeRecord
  :: ( D.GDecodeJson (Either String) Record Nil () l r
     , RowToList r l
     )
  => DecodeJson (Record r)
  where
  decodeJson json =
    case toObject json of
      Just object ->
        D.gDecodeJson
          (RLProxy :: RLProxy Nil)
          (RLProxy :: RLProxy l)
          object
          {}
      Nothing ->
        reportError "Could not convert JSON to object"

else instance decodeDecodeJson :: D.DecodeJson a => DecodeJson a where
  decodeJson = D.decodeJson
