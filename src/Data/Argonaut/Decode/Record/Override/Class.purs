module Data.Argonaut.Decode.Record.Override.Class
  ( class DecodeJsonWith
  , decodeJsonWith
  ) where

import Prelude (class Bind, bind, ($))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Record.Utils (getMissingFieldErrorMessage)
import Data.EmptyRow (class EmptyRow, emptyRow)
import Data.Maybe (Maybe(Just, Nothing))
import Data.SameKeys (class SameKeys)
import Data.Status (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
import Record (get, insert)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Equality (class TypeEquals, to)
import Type.Row
  ( class Cons
  , class Lacks
  , class RowToList
  , Cons
  , Nil
  , kind RowList
  )
import Unsafe.Coerce (unsafeCoerce)

class DecodeJsonWith
  (f :: Type -> Type)
  (g :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0 l1 r1 where
  decodeJsonWith
    :: RLProxy l0
    -> g r0
    -> Object Json
    -> f (g r1)

instance decodeJsonWithNil
  :: ( EmptyRow g
     , Status f
     )
  => DecodeJsonWith f g Nil () Nil () where
  decodeJsonWith _ _ _ = report emptyRow

instance decodeJsonWithCons
  :: ( Bind f
     , SameKeys dl r
     , SameKeys dl' r'
     , Cons s v r' r
     , Cons s dv dr' dr
     , DecodeJsonWith f Record dl' dr' l' r'
     , IsSymbol s
     , Lacks s r'
     , Lacks s dr'
     , RowToList r l
     , RowToList r' l'
     , RowToList dr dl
     , RowToList dr' dl'
     , Status f
     , TypeEquals dv (Json -> f v)
     )
  => DecodeJsonWith f Record (Cons s dv dl') dr (Cons s v l') r
  where
  decodeJsonWith _ decoderRecord object = do
    case lookup fieldName object of
      Just jsonVal -> do
        rest <- decodeJsonWith dl_ decoderRecord' object
        val <- decoder jsonVal
        report $ insert s val rest
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName
    where
    decoder :: Json -> f v
    decoder = to $ get s decoderRecord

    -- To prevent unnecessary creation of intermediate decoder records,
    -- coercion is used rather than calling `Record.delete s`
    -- to induce the next expected type.
    decoderRecord' :: Record dr'
    decoderRecord' = unsafeCoerce decoderRecord

    dl_ :: RLProxy dl'
    dl_ = RLProxy

    fieldName :: String
    fieldName = reflectSymbol s

    s :: SProxy s
    s = SProxy
