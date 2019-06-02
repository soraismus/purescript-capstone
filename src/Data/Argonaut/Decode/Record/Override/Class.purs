module Data.Argonaut.Decode.Record.Override.Class
  ( class DecodeJsonWith
  , decodeJsonWith
  ) where

import Prelude (class Bind, bind, ($))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Record.Utils (getMissingFieldErrorMessage)
import Data.RecordLike (class RGet, class RInsert, rget, rinsert)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Equality (class TypeEquals, to)
import Type.Row
  ( class Cons
  , class Lacks
  , Cons
  , Nil
  , kind RowList
  )
import Unsafe.Coerce (unsafeCoerce)

class DecodeJsonWith
  (f  :: Type -> Type)
  (g  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (r2 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l0 l1 -> r2
  where
  decodeJsonWith
    :: RLProxy l0
    -> RLProxy l1
    -> g r0
    -> Object Json
    -> g r1
    -> f (g r2)

instance decodeJsonWithNil
  :: Status f
  => DecodeJsonWith f g Nil () l r r
  where
  decodeJsonWith _ _ _ _ = report

instance decodeJsonWithCons
  :: ( Bind f
     , Cons s fn r0' r0
     , Cons s v r2' r2
     , DecodeJsonWith f g l0' r0' l1 r1 r2'
     , IsSymbol s
     , Status f
     , Lacks s r2'
     , RGet g SProxy s l0 r0
     , RInsert Function g SProxy s l2' r2' l2 r2
     , TypeEquals fn (Json -> f v)
     )
  => DecodeJsonWith f g (Cons s fn l0') r0 l1 r1 r2
  where
  decodeJsonWith _ _ decoderRecord object record = do
    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal
        intermediate <- decodeJsonWith l0' l1 decoderRecord' object record
        report $ rinsert l2' l2 s val intermediate
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName
    where
    decoder :: Json -> f v
    decoder = to $ rget l0 s decoderRecord

    -- To prevent unnecessary creation of intermediate decoder records,
    -- coercion is used rather than calling `Record.delete s`
    -- to induce the next expected type.
    decoderRecord' :: g r0'
    decoderRecord' = unsafeCoerce decoderRecord

    fieldName :: String
    fieldName = reflectSymbol s

    l0 :: RLProxy l0
    l0 = RLProxy

    l0' :: RLProxy l0'
    l0' = RLProxy

    l1 :: RLProxy l1
    l1 = RLProxy

    l2 :: RLProxy l2
    l2 = RLProxy

    l2' :: RLProxy l2'
    l2' = RLProxy

    s :: SProxy s
    s = SProxy
