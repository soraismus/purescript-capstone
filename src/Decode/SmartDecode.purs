module Data.Argonaut.Decode.Smart where

import Prelude (bind, ($))

import Control.Plus (class Plus, empty)
import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode.Class
  ( class DecodeJson
  --, class GDecodeJson
  , decodeJson
  --, gDecodeJson
  )
import Data.Argonaut.Utils (getMissingFieldErrorMessage)
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
  , class Union
  , Cons
  , Nil
  , RProxy(RProxy)
  , kind RowList
  )

foreign import unsafeCreate :: forall r0 a. String -> a -> Record r0

createSingletonRecord
  :: forall l r s v
   . IsSymbol s
  => ListToRow l r
  => TypeEquals (RLProxy l) (RLProxy (Cons s v Nil))
  => SProxy s
  -> v
  -> Record r
createSingletonRecord sProxy value =
  unsafeCreate (reflectSymbol sProxy) value

class SmartDecodeJson a where
  smartDecodeJson :: Json -> Either String a

instance smartDecodeRecord
  :: ( GSmartDecodeJson r0 l0 r1 l1 r2 l2
     --, RowToList r0 l0
     --, RowToList r1 l1
     , RowToList r2 l2
     --, Union r0 r1 r2
     )
  => SmartDecodeJson (Record r2)
  where
  smartDecodeJson json =
    case toObject json of
      Just object ->
        gSmartDecodeJson
          object
          (RLProxy :: RLProxy l0)
          (RLProxy :: RLProxy l1)
          (RLProxy :: RLProxy l2)
      Nothing ->
        Left "Could not convert JSON to object"

else instance smartDecodeDecodeJson
  :: DecodeJson a
  => SmartDecodeJson a
  where
  smartDecodeJson = decodeJson

class GSmartDecodeJson
  (r0 :: # Type)
  (l0 :: RowList)
  (r1 :: # Type)
  (l1 :: RowList)
  (r2 :: # Type)
  (l2 :: RowList)
  | l0 -> r0
  , l1 -> r1
  , l2 -> r2
  where
  gSmartDecodeJson
    :: Object Json
    -> RLProxy l0
    -> RLProxy l1
    -> RLProxy l2
    -> Either String (Record r2)

--instance gSmartDecodeJsonNil :: GSmartDecodeJson () Nil () Nil () Nil
instance gSmartDecodeJsonNil :: GSmartDecodeJson r0 l0 r1 l1 () Nil
  where
  gSmartDecodeJson _ _ _ _ = Right {}

instance gSmartDecodeJsonCons_Plus_1
  :: ( DecodeJson (f v)
     , IsSymbol s
     , ListToRow (Cons s (f v) Nil) r2
     , Plus f
     )
  => GSmartDecodeJson r0 l0 r1 l1 r2 (Cons s (f v) Nil)
  where
  gSmartDecodeJson object _ _ _ = do
    let
      sProxy :: SProxy s
      sProxy = SProxy
      fieldName :: String
      fieldName = reflectSymbol sProxy
    case lookup fieldName object of
      Just jsonVal -> do
        (val :: f v) <- decodeJson jsonVal
        Right $ createSingletonRecord sProxy val
      Nothing ->
        Right $ createSingletonRecord sProxy (empty :: f v)


else instance gSmartDecodeJsonCons_nonPlus_1
  :: ( DecodeJson v
     , IsSymbol s
     , ListToRow (Cons s v Nil) r2
     )
  => GSmartDecodeJson r0 l0 r1 l1 r2 (Cons s v Nil)
  where
  gSmartDecodeJson object _ _ _ = do
    let
      sProxy :: SProxy s
      sProxy = SProxy
      fieldName :: String
      fieldName = reflectSymbol sProxy
    case lookup fieldName object of
      Just jsonVal -> do
        (val :: v) <- decodeJson jsonVal
        Right $ createSingletonRecord sProxy val
      Nothing ->
        Left $ getMissingFieldErrorMessage fieldName

instance gSmartDecodeJsonCons_Plus
  :: ( Cons s (f v) r1' r1
     , Cons s (f v) r2' r2
     , DecodeJson (f v)
     , GSmartDecodeJson r0 l0 r1' l1' r2' (Cons s2' v2' l2'')
     , IsSymbol s
     , Lacks s r1'
     , Lacks s r2'
     , Plus f
     , TypeEquals (RLProxy l1) (RLProxy (Cons s v l1'))
     , Union r0 r1 r2
     , Union r0 r1' r2'
     )
  => GSmartDecodeJson
        r0
        l0
        r1
        l1
        r2
        (Cons s (f v) (Cons s2' v2' l2''))
  where
  gSmartDecodeJson object _ _ _ = do
    let
      sProxy :: SProxy s
      sProxy = SProxy
      fieldName :: String
      fieldName = reflectSymbol sProxy
    (rest :: Record r2') <- gSmartDecodeJson
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

else instance gSmartDecodeJsonCons_nonPlus
  :: ( Cons s v r0' r0
     , Cons s v r2' r2
     , DecodeJson v
     , GSmartDecodeJson r0' l0' r1 l1 r2' (Cons s2' v2' l2'')
     , IsSymbol s
     , Lacks s r0'
     , Lacks s r2'
     , TypeEquals (RLProxy l0) (RLProxy (Cons s v l0'))
     , Union r0  r1 r2
     , Union r0' r1 r2'
     )
  => GSmartDecodeJson
        r0
        l0
        r1
        l1
        r2
        (Cons s v (Cons s2' v2' l2''))
  where
  gSmartDecodeJson object _ _ _ = do
    let
      sProxy :: SProxy s
      sProxy = SProxy
      fieldName :: String
      fieldName = reflectSymbol sProxy
    (rest :: Record r2') <- gSmartDecodeJson
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
