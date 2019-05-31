module Data.Argonaut.Decode.Record.Cross.Class
  ( class DecodeJsonWith
  , decodeJsonWith
  ) where

import Prelude (class Bind, bind, identity, ($), (<<<))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Record.Utils (getMissingFieldErrorMessage)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
import Record (get, insert)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Equality (class TypeEquals, to)
import Type.Row
  ( class Cons
  , class Lacks
  , class Nub
  , class Union
  , Cons
  , Nil
  , kind RowList
  )
import Unsafe.Coerce (unsafeCoerce)

class DecodeJsonWith
  (f :: Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (r1 :: # Type)
  (l2 :: RowList)
  (r2 :: # Type)
  (r3 :: # Type)
  a
  | l0 -> r0 a r1
  , l2 -> r2
  , l0 l2 -> r3
  where
  decodeJsonWith
    :: RLProxy l0
    -> RLProxy l2
    -> Record r0
    -> Object Json
    -> a
    -> f (Record r2 -> Record r3)

instance decodeJsonWithNil
  :: Status f
  => DecodeJsonWith f Nil () () l r r a
  where
  decodeJsonWith _ _ _ _ _ = report identity

instance decodeJsonWithCons
  :: ( Bind f
     , Cons s fn r0' r0
     , Cons s v r1' r1
     , Cons s v r3' r3
     , DecodeJsonWith f l0' r0' r1' l2 r2 r3' a
     , IsSymbol s
     , Status f
     , Lacks s r1'
     , Lacks s r3'
     , Nub r3 r3
     , TypeEquals fn (Json -> a -> f v)
     , Union r1 r2 r3
     )
  => DecodeJsonWith f (Cons s fn l0') r0 r1 l2 r2 r3 a
  where
  decodeJsonWith _ _ decoderRecord object x = do
    let
      sProxy :: SProxy s
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

      decoder :: Json -> a -> f v
      decoder = to $ get sProxy decoderRecord

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy`
      -- to induce the next expected type.
      decoderRecord' :: Record r0'
      decoderRecord' = unsafeCoerce decoderRecord

    doRest <-
      decodeJsonWith
        (RLProxy :: RLProxy l0')
        (RLProxy :: RLProxy l2)
        decoderRecord'
        object
        x

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal x
        report $ insert sProxy val <<< doRest
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName
