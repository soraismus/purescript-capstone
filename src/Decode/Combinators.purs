module Data.Argonaut.Decode.Smart.Combinators
  ( getFieldTolerantly
  , getFieldOptionalTolerantly
  , getFieldOptionalTolerantly'
  , (.::)
  , (.::!)
  , (.::?)
  ) where

import Prelude

-- | Attempt to get the value for a given key on an `Object Json`.
-- |
-- | Use this accessor if the key and value *must* be present in your object.
-- | If the key and value are optional, use `getFieldOptionalTolerantly'` (`.::?`) instead.
getFieldTolerantly
  :: forall a
   . SmartDecodeJson a
  => FO.Object Json
  -> String
  -> Either String a
getFieldTolerantly o s =
  maybe
    (Left $ "Expected field " <> show s)
    (elaborateFailure s <<< smartDecodeJson)
    (FO.lookup s o)

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
   . DecodeJson a
  => FO.Object Json
  -> String
  -> Either String (Maybe a)
getFieldOptionalTolerantly' o s =
  maybe
    (pure Nothing)
    decode
    (FO.lookup s o)
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
   . DecodeJson a
  => FO.Object Json
  -> String
  -> Either String (Maybe a)
getFieldOptionalTolerantly o s =
  maybe
    (pure Nothing)
    decode
    (FO.lookup s o)
  where
    decode json = Just <$> (elaborateFailure s <<< smartDecodeJson) json

infix 7 getFieldOptionalTolerantly as .::!
