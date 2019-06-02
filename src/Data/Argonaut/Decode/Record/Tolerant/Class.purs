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
  , class Union
  , Cons
  , Nil
  , kind RowList
  )

class DecodeJson a where
  decodeJson :: Json -> Either String a

instance decodeRecord
  :: ( GDecodeJson (Either String) Record l r Nil () l r
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
  (l2 :: RowList)
  (r2 :: # Type)
  | l1 -> r1
  , l2 -> r2
  , l1 l2 -> l0 r0
  where
  gDecodeJson
    :: RLProxy l1
    -> RLProxy l2
    -> Object Json
    -> g r1
    -> h (g r2)

instance gDecodeJson_NilNilNil
  :: Status h
  => GDecodeJson h g Nil () Nil () Nil () where
  gDecodeJson _ _ _ = report

instance gDecodeJson_ConsNilCons_Plus
  :: ( Cons s (f v) r' r
     , D.DecodeJson (f v)
     , GDecodeJson (Either String) g l' r' Nil () l' r'
     , IsSymbol s
     , Lacks s r'
     , Plus f
     , RInsert g SProxy s l' r' l r
     )
  => GDecodeJson
        (Either String)
        g
        (Cons s (f v) l')
        r
        Nil
        ()
        (Cons s (f v) l')
        r
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
     , GDecodeJson (Either String) g l' r' Nil () l' r'
     , IsSymbol s
     , Lacks s r'
     , RInsert g SProxy s l' r' l r
     )
  => GDecodeJson
        (Either String)
        g
        (Cons s v l')
        r
        Nil
        ()
        (Cons s v l')
        r
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
  => GDecodeJson h g Nil () (Cons s v l') r (Cons s v l') r
  where
  gDecodeJson _ _ _ = report

else instance gDecodeJson_ConsConsCons_Plus
  :: ( Cons s (f v) r0' r0
     , Cons s (f v) r2' r2
     , D.DecodeJson (f v)
     , GDecodeJson
          (Either String)
          g
          l0'
          r0'
          (Cons s1 v1 l1')
          r1
          l2'
          r2'
     , IsSymbol s
     , Lacks s r1
     , Lacks s r2'
     , Plus f
     , RInsert g SProxy s l2' r2' l2 r2
     , Union r0 r1 r2
     )
  => GDecodeJson
        (Either String)
        g
        (Cons s (f v) l0')
        r0
        (Cons s1 v1 l1')
        r1
        (Cons s (f v) l2')
        r2
  where
  gDecodeJson _ _ object record = do
    intermediate <- gDecodeJson l1 l2' object record
    case lookup fieldName object of
      Just jsonVal -> do
        (val :: f v) <- D.decodeJson jsonVal
        report $ rinsert l2' l2 s val intermediate
      Nothing ->
        report $ rinsert l2' l2 s (empty :: f v) intermediate
    where
    fieldName :: String
    fieldName = reflectSymbol s

    l1 :: RLProxy (Cons s1 v1 l1')
    l1 = RLProxy

    l2' :: RLProxy l2'
    l2' = RLProxy

    l2 :: RLProxy l2
    l2 = RLProxy

    s :: SProxy s
    s = SProxy


else instance gDecodeJson_ConsConsCons_nonPlus
  :: ( Cons s v r0' r0
     , Cons s v r2' r2
     , D.DecodeJson v
     , GDecodeJson
          (Either String)
          g
          l0'
          r0'
          (Cons s1 v1 l1')
          r1
          l2'
          r2'
     , IsSymbol s
     , Lacks s r1
     , Lacks s r2'
     , RInsert g SProxy s l2' r2' l2 r2
     , Union r0 r1 r2
     )
  => GDecodeJson
        (Either String)
        g
        (Cons s v l0')
        r0
        (Cons s1 v1 l1')
        r1
        (Cons s v l2')
        r2
  where
  gDecodeJson _ _ object record = do
    case lookup fieldName object of
      Just jsonVal -> do
        val <- D.decodeJson jsonVal
        intermediate <- gDecodeJson l1 l2' object record
        report $ rinsert l2' l2 s val intermediate
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName
    where
    fieldName :: String
    fieldName = reflectSymbol s

    l1 :: RLProxy (Cons s1 v1 l1')
    l1 = RLProxy

    l2' :: RLProxy l2'
    l2' = RLProxy

    l2 :: RLProxy l2
    l2 = RLProxy

    s :: SProxy s
    s = SProxy
