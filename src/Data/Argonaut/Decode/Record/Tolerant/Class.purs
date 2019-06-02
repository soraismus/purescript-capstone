module Data.Argonaut.Decode.Record.Tolerant.Class
  ( class DecodeJson
  , class GDecodeJson
  , decodeJson
  , gDecodeJson
  ) where

import Prelude (bind, ($))

import Control.Plus (class Plus, empty)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson) as D
import Data.Argonaut.Decode.Record.Utils (getMissingFieldErrorMessage)
import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing))
import Data.RecordLike (class RInsert, rinsert)
import Data.Status (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row
  ( class Cons
  , class Lacks
  , class RowToList
  , Cons
  , Nil
  , kind RowList
  )

class DecodeJson a where
  decodeJson :: Json -> Either String a

instance decodeRecord
  :: ( GDecodeJson (Either String) Record Nil () l r
     , RowToList r l
     )
  => DecodeJson (Record r)
  where
  decodeJson json =
    case toObject json of
      Just object ->
        gDecodeJson
          (RLProxy :: RLProxy Nil)
          (RLProxy :: RLProxy l)
          object
          {}
      Nothing ->
        reportError "Could not convert JSON to object"

else instance decodeDecodeJson :: D.DecodeJson a => DecodeJson a where
  decodeJson = D.decodeJson

class GDecodeJson
  (h  :: Type -> Type)
  (g  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  gDecodeJson
    :: RLProxy l0
    -> RLProxy l1
    -> Object Json
    -> g r0
    -> h (g r1)

instance gDecodeJson_NilNilNil
  :: Status h
  => GDecodeJson h g Nil () Nil () where
  gDecodeJson _ _ _ = report

instance gDecodeJson_ConsNilCons_Plus
  :: ( Cons s (f v) r' r
     , D.DecodeJson (f v)
     , GDecodeJson (Either String) g Nil () l' r'
     , IsSymbol s
     , Lacks s r'
     , Plus f
     , RInsert g SProxy s l' r' l r
     )
  => GDecodeJson (Either String) g Nil () (Cons s (f v) l') r
  where
  gDecodeJson _ _ object record = do
    intermediate <- gDecodeJson nil l' object record
    case lookup fieldName object of
      Just jsonVal -> do
        (val :: f v) <- D.decodeJson jsonVal
        report $ rinsert l' l s val intermediate
      Nothing ->
        report $ rinsert l' l s (empty :: f v) intermediate
    where
    fieldName :: String
    fieldName = reflectSymbol s

    l' :: RLProxy l'
    l' = RLProxy

    l :: RLProxy l
    l = RLProxy

    nil :: RLProxy Nil
    nil = RLProxy

    s :: SProxy s
    s = SProxy

else instance gDecodeJson_ConsNilCons_nonPlus
  :: ( Cons s v r' r
     , D.DecodeJson v
     , GDecodeJson (Either String) g Nil () l' r'
     , IsSymbol s
     , Lacks s r'
     , RInsert g SProxy s l' r' l r
     )
  => GDecodeJson (Either String) g Nil () (Cons s v l') r
  where
  gDecodeJson _ _ object record = do
    case lookup fieldName object of
      Just jsonVal -> do
        val <- D.decodeJson jsonVal
        intermediate <- gDecodeJson nil l' object record
        report $ rinsert l' l s val intermediate
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName
    where
    fieldName :: String
    fieldName = reflectSymbol s

    l' :: RLProxy l'
    l' = RLProxy

    l :: RLProxy l
    l = RLProxy

    nil :: RLProxy Nil
    nil = RLProxy

    s :: SProxy s
    s = SProxy


instance gDecodeJson_NilConsCons
  :: Status h
  => GDecodeJson h g (Cons s v l') r (Cons s v l') r
  where
  gDecodeJson _ _ _ = report

else instance gDecodeJson_ConsConsCons_Plus
  :: ( Cons s (f v) r1' r1
     , D.DecodeJson (f v)
     , GDecodeJson (Either String) g (Cons s1 v1 l0') r0 l1' r1'
     , IsSymbol s
     , Lacks s r0
     , Lacks s r1'
     , Plus f
     , RInsert g SProxy s l1' r1' l1 r1
     )
  => GDecodeJson (Either String) g (Cons s1 v1 l0') r0 (Cons s (f v) l1') r1
  where
  gDecodeJson _ _ object record = do
    intermediate <- gDecodeJson l0 l1' object record
    case lookup fieldName object of
      Just jsonVal -> do
        (val :: f v) <- D.decodeJson jsonVal
        report $ rinsert l1' l1 s val intermediate
      Nothing ->
        report $ rinsert l1' l1 s (empty :: f v) intermediate
    where
    fieldName :: String
    fieldName = reflectSymbol s

    l0 :: RLProxy (Cons s1 v1 l0')
    l0 = RLProxy

    l1' :: RLProxy l1'
    l1' = RLProxy

    l1 :: RLProxy l1
    l1 = RLProxy

    s :: SProxy s
    s = SProxy


else instance gDecodeJson_ConsConsCons_nonPlus
  :: ( Cons s v r1' r1
     , D.DecodeJson v
     , GDecodeJson (Either String) g (Cons s1 v1 l0') r0 l1' r1'
     , IsSymbol s
     , Lacks s r0
     , Lacks s r1'
     , RInsert g SProxy s l1' r1' l1 r1
     )
  => GDecodeJson (Either String) g (Cons s1 v1 l0') r0 (Cons s v l1') r1
  where
  gDecodeJson _ _ object record = do
    case lookup fieldName object of
      Just jsonVal -> do
        val <- D.decodeJson jsonVal
        intermediate <- gDecodeJson l0 l1' object record
        report $ rinsert l1' l1 s val intermediate
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName
    where
    fieldName :: String
    fieldName = reflectSymbol s

    l0 :: RLProxy (Cons s1 v1 l0')
    l0 = RLProxy

    l1' :: RLProxy l1'
    l1' = RLProxy

    l1 :: RLProxy l1
    l1 = RLProxy

    s :: SProxy s
    s = SProxy
