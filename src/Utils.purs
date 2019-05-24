module Data.Argonaut.Utils
  ( getMissingFieldErrorMessage
  , reportJson
  , reportObject
  ) where

import Prelude

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class (class GDecodeJson, gDecodeJson)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status.Class (class Status, report, reportError)
import Foreign.Object (Object)
import Type.Data.RowList (RLProxy) -- Argonaut dependency
import Type.Row (class RowToList)

notObjectErrorMessage :: String
notObjectErrorMessage = "Could not convert JSON to object"

getMissingFieldErrorMessage :: String -> String
getMissingFieldErrorMessage fieldName =
  "JSON was missing expected field: " <> fieldName

reportJson
  :: forall f r
   . Status f
  => (Object Json -> f (Record r))
  -> Json
  -> f (Record r)
reportJson f json =
  case toObject json of
    Just object -> f object
    Nothing -> reportError notObjectErrorMessage

reportObject
  :: forall f l r
   . GDecodeJson r l
  => RowToList r l
  => Status f
  => Object Json
  -> RLProxy l
  -> f (Record r)
reportObject object rlProxy =
  case gDecodeJson object rlProxy of
    Left errorStr -> reportError errorStr
    Right record -> report record
