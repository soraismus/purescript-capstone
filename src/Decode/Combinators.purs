module Data.Argonaut.Decode.Smart.Combinators
  ( getFieldTolerantly
  , getFieldOptionalTolerantly
  , getFieldOptionalTolerantly'
  , (.::)
  , (.::!)
  , (.::?)
  ) where

import Prelude

import Data.Argonaut.Core (Json, isNull)
import Data.Argonaut.Decode.Smart.Class (class SmartDecodeJson, smartDecodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Foreign.Object (Object, lookup)

-- | Attempt to get the value for a given key on an `Object Json`.
-- |
-- | Use this accessor if the key and value *must* be present in your object.
-- | If the key and value are optional, use `getFieldOptionalTolerantly'` (`.::?`) instead.
getFieldTolerantly
  :: forall a
   . SmartDecodeJson a
  => Object Json
  -> String
  -> Either String a
getFieldTolerantly o s =
  maybe
    (Left $ "Expected field " <> show s)
    (elaborateFailure s <<< smartDecodeJson)
    (lookup s o)

infix 7 getFieldTolerantly as .::

-- | Attempt to get the value for a given key on an `Object Json`.
-- |
-- | The result will be `Right Nothing` if the key and value are not present,
-- | or if the key is present and the value is `null`.
-- |
-- | Use this accessor if the key and value are optional in your object.
-- | If the key and value are mandatory, use `getFieldTolerantly` (`.::`) instead.
getFieldOptionalTolerantly'
  :: forall a
   . SmartDecodeJson a
  => Object Json
  -> String
  -> Either String (Maybe a)
getFieldOptionalTolerantly' o s =
  maybe
    (pure Nothing)
    decode
    (lookup s o)
  where
    decode json =
      if isNull json
        then pure Nothing
        else Just <$> (elaborateFailure s <<< smartDecodeJson) json

infix 7 getFieldOptionalTolerantly' as .::?

-- | Attempt to get the value for a given key on an `Object Json`.
-- |
-- | The result will be `Right Nothing` if the key and value are not present,
-- | but will fail if the key is present but the value cannot be converted to the right type.
-- |
-- | This function will treat `null` as a value and attempt to decode it into your desired type.
-- | If you would like to treat `null` values the same as absent values, use
-- | `getFieldOptionalTolerantly'` (`.::?`) instead.
getFieldOptionalTolerantly
  :: forall a
   . SmartDecodeJson a
  => Object Json
  -> String
  -> Either String (Maybe a)
getFieldOptionalTolerantly o s =
  maybe
    (pure Nothing)
    decode
    (lookup s o)
  where
    decode json = Just <$> (elaborateFailure s <<< smartDecodeJson) json

infix 7 getFieldOptionalTolerantly as .::!

elaborateFailure :: ∀ a. String -> Either String a -> Either String a
elaborateFailure s e =
  lmap msg e
  where
    msg m = "Failed to decode key '" <> s <> "': " <> m
