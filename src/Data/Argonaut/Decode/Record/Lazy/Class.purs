module Data.Argonaut.Decode.Record.Lazy.Class
  ( class GDecodeJson
  , gDecodeJson
  ) where

import Prelude (bind, ($), (=<<))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson) as D
import Data.Argonaut.Decode.Record.Utils (getMissingFieldErrorMessage)
import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status (report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
import Record (insert)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row
  ( class Cons
  , class Lacks
  , class Union
  , Cons
  , Nil
  , kind RowList
  )

class GDecodeJson
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
    -> Record r1
    -> Either String (Record r2)

instance gDecodeJson_NilNilNil :: GDecodeJson Nil () Nil () Nil () where
  gDecodeJson _ _ _ = report

instance gDecodeJson_ConsNilCons
  :: ( Cons s v r' r
     , D.DecodeJson v
     , GDecodeJson l' r' Nil () l' r'
     , IsSymbol s
     , Lacks s r'
     )
  => GDecodeJson (Cons s v l') r Nil () (Cons s v l') r
  where
  gDecodeJson _ _ object record = do
    let
      sProxy :: SProxy s
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

    (x :: Record r') <-
      gDecodeJson
        (RLProxy :: RLProxy Nil)
        (RLProxy :: RLProxy l')
        object
        record

    case lookup fieldName object of
      Just jsonVal -> do
        val <- D.decodeJson jsonVal
        report $ insert sProxy val x
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName

instance gDecodeJson_NilConsCons
  :: GDecodeJson Nil () (Cons s v l') r (Cons s v l') r
  where
  gDecodeJson _ _ _ = report

else instance gDecodeJson_ConsConsCons
  :: ( Cons s v r0' r0
     , Cons s v r2' r2
     , D.DecodeJson v
     , GDecodeJson l0' r0' (Cons s1 v1 l1') r1 l2' r2'
     , IsSymbol s
     , Lacks s r1
     , Lacks s r2'
     , Union r0 r1 r2
     )
  => GDecodeJson (Cons s v2 l0') r0 (Cons s1 v1 l1') r1 (Cons s v2 l2') r2
  where
  gDecodeJson _ _ object record = do
    let
      sProxy :: SProxy s
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

    (x :: Record r2') <-
      gDecodeJson
        (RLProxy :: RLProxy (Cons s1 v1 l1'))
        (RLProxy :: RLProxy l2')
        object
        record

    case lookup fieldName object of
      Just jsonVal -> do
        val <- D.decodeJson jsonVal
        report $ insert sProxy val x
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName
