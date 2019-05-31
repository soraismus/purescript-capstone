module Data.Argonaut.Decode.Record.Tolerant.Class
  ( class DecodeJson
  , class GDecodeJson
  , decodeJson
  , gDecodeJson
  ) where

import Prelude (bind, ($))

import Control.Plus (class Plus, empty)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class
  ( class DecodeJson
  , class GDecodeJson
  , decodeJson
  ) as D
import Data.Argonaut.Decode.Record.Utils
  ( getMissingFieldErrorMessage
  , singleton
  )
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
import Record (insert)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Equality (class TypeEquals)
import Type.Prelude (class ListToRow)
import Type.Row
  ( class Cons
  , class Lacks
  , class RowToList
  , Cons
  , Nil
  , kind RowList
  )

class D.DecodeJson a <= DecodeJson a where
  decodeJson :: Json -> Either String a

instance decodeRecord
  :: ( GDecodeJson l0 l1 l2 r2
     , RowToList r2 l2
     )
  => DecodeJson (Record r2)
  where
  decodeJson json =
    case toObject json of
      Just object ->
        gDecodeJson
          object
          (RLProxy :: RLProxy l0)
          (RLProxy :: RLProxy l1)
          (RLProxy :: RLProxy l2)
      Nothing ->
        Left "Could not convert JSON to object"

else instance decodeDecodeJson :: D.DecodeJson a => DecodeJson a where
  decodeJson = D.decodeJson

class D.GDecodeJson r2 l2 <= GDecodeJson
  (l0 :: RowList)
  (l1 :: RowList)
  (l2 :: RowList)
  (r2 :: # Type)
  | l2 -> r2
  where
  gDecodeJson
    :: Object Json
    -> RLProxy l0
    -> RLProxy l1
    -> RLProxy l2
    -> Either String (Record r2)

instance gDecodeJson_Nil :: GDecodeJson l0 l1 Nil () where
  gDecodeJson _ _ _ _ = Right {}

instance gDecodeJson_ConsOne_Plus
  :: ( Cons s (f v) () r2
     , IsSymbol s
     , Lacks s ()
     , ListToRow (Cons s (f v) Nil) r2
     , Plus f
     , DecodeJson (f v)
     )
  => GDecodeJson l0 l1 (Cons s (f v) Nil) r2
  where
  gDecodeJson object _ _ _ = do
    let
      sProxy :: SProxy s
      sProxy = SProxy
      fieldName :: String
      fieldName = reflectSymbol sProxy
    case lookup fieldName object of
      Just jsonVal -> do
        (val :: f v) <- decodeJson jsonVal
        Right $ singleton sProxy val
      Nothing ->
        Right $ singleton sProxy (empty :: f v)

else instance gDecodeJson_ConsOne_nonPlus
  :: ( Cons s v () r2
     , IsSymbol s
     , Lacks s ()
     , ListToRow (Cons s v Nil) r2
     , DecodeJson v
     )
  => GDecodeJson l0 l1 (Cons s v Nil) r2
  where
  gDecodeJson object _ _ _ = do
    let
      sProxy :: SProxy s
      sProxy = SProxy
      fieldName :: String
      fieldName = reflectSymbol sProxy
    case lookup fieldName object of
      Just jsonVal -> do
        (val :: v) <- decodeJson jsonVal
        Right $ singleton sProxy val
      Nothing ->
        Left $ getMissingFieldErrorMessage fieldName

instance gDecodeJson_ConsMany_Plus
  :: ( Cons s (f v) r1' r1
     , Cons s (f v) r2' r2
     , Cons s2' v2' r2'' r2'
     , D.GDecodeJson r2'' l2''
     , GDecodeJson l0 l1' (Cons s2' v2' l2'') r2'
     , IsSymbol s
     , IsSymbol s2'
     , Lacks s r2'
     , Lacks s2' r2''
     , Plus f
     , DecodeJson (f v)
     , DecodeJson v2'
     , TypeEquals (RLProxy l1) (RLProxy (Cons s v l1'))
     )
  => GDecodeJson l0 l1 (Cons s (f v) (Cons s2' v2' l2'')) r2
  where
  gDecodeJson object _ _ _ = do
    let
      sProxy :: SProxy s
      sProxy = SProxy
      fieldName :: String
      fieldName = reflectSymbol sProxy
    (rest :: Record r2') <- gDecodeJson
              object
              (RLProxy :: RLProxy l0)
              (RLProxy :: RLProxy l1')
              (RLProxy :: RLProxy (Cons s2' v2' l2''))
    case lookup fieldName object of
      Just jsonVal -> do
        (val :: f v) <- decodeJson jsonVal
        Right $ insert sProxy val rest
      Nothing ->
        Right $ insert sProxy empty rest

else instance gDecodeJson_ConsMany_nonPlus
  :: ( Cons s v r0' r0
     , Cons s v r2' r2
     , Cons s2' v2' r2'' r2'
     , D.GDecodeJson r2'' l2''
     , GDecodeJson l0' l1 (Cons s2' v2' l2'') r2'
     , IsSymbol s
     , IsSymbol s2'
     , Lacks s r2'
     , Lacks s2' r2''
     , DecodeJson v
     , DecodeJson v2'
     , TypeEquals (RLProxy l0) (RLProxy (Cons s v l0'))
     )
  => GDecodeJson l0 l1 (Cons s v (Cons s2' v2' l2'')) r2
  where
  gDecodeJson object _ _ _ = do
    let
      sProxy :: SProxy s
      sProxy = SProxy
      fieldName :: String
      fieldName = reflectSymbol sProxy
    (rest :: Record r2') <- gDecodeJson
              object
              (RLProxy :: RLProxy l0')
              (RLProxy :: RLProxy l1)
              (RLProxy :: RLProxy (Cons s2' v2' l2''))
    case lookup fieldName object of
      Just jsonVal -> do
        (val :: v) <- decodeJson jsonVal
        Right $ insert sProxy val rest
      Nothing ->
        Left $ getMissingFieldErrorMessage fieldName
