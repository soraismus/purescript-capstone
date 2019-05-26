module Data.Argonaut.Decode.Record.Cross2.Class
  ( class DecodeJsonWith
  , decodeJsonWith
  ) where

import Prelude (class Bind, bind, ($))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Record.Utils (getMissingFieldErrorMessage)
import Data.Cases1 (class Cases1)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Status (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Data.Tuple (Tuple(Tuple))
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
  (l1 :: RowList)
  (r1 :: # Type)
  (l0 :: RowList)
  (r0 :: # Type)
  a
  | l1 -> r1 l0 r0 a where
  decodeJsonWith
    :: RLProxy l0
    -> RLProxy l1
    -> Record r1
    -> Object Json
    -> a
    -> f (Record r0)

instance decodeJsonWithNil
  :: Status f
  => DecodeJsonWith f Nil () Nil () a where
  decodeJsonWith _ _ _ _ _ = report {}

instance decodeJsonWithCons
  :: ( Bind f
     , Cases1 f dl r a
     , Cases1 f dl' r' a
     , Cons s v r' r
     , Cons ds dv dr' dr
     , DecodeJsonWith f dl' dr' l' r' a
     , IsSymbol s
     , IsSymbol ds
     , Lacks s r'
     , Lacks ds dr'
     , RowToList r l
     , RowToList r' l'
     , RowToList dr dl
     , RowToList dr' dl'
     , Status f
     , TypeEquals dv (Json -> a -> f (Tuple v (SProxy s)))
     )
  => DecodeJsonWith f (Cons ds dv dl') dr (Cons s v l') r a
  where
  decodeJsonWith _ _ decoderRecord object x = do
    let
      oldSProxy :: SProxy ds
      oldSProxy = SProxy

      newSProxy :: SProxy s
      newSProxy = SProxy

      oldFieldName :: String
      oldFieldName = reflectSymbol oldSProxy

      decoder :: Json -> a -> f (Tuple v (SProxy s))
      decoder = to $ get oldSProxy decoderRecord

      -- To prevent unnecessary creation of intermediate decoder records,
      -- coercion is used rather than calling `Record.delete sProxy`
      -- to induce the next expected type.
      decoderRecord' :: Record dr'
      decoderRecord' = unsafeCoerce decoderRecord

    rest <-
      decodeJsonWith
        (RLProxy :: RLProxy l')
        (RLProxy :: RLProxy dl')
        decoderRecord'
        object
        x

    case lookup oldFieldName object of
      Just jsonVal -> do
        Tuple val _ <- decoder jsonVal x
        report $ insert newSProxy val rest
      Nothing ->
        reportError $ getMissingFieldErrorMessage oldFieldName
