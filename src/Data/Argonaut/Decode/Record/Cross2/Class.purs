module Data.Argonaut.Decode.Record.Cross2.Class
  ( RenameField(RenameField)
  , class DecodeJsonWith
  , decodeJsonWith
  ) where

import Prelude (class Bind, bind, ($))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Record.Utils
  ( getMissingFieldErrorMessage
  , singleton
  )
import Data.Maybe (Maybe(Just, Nothing))
import Data.SameSize1 (class SameSize1)
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

data RenameField (sym :: Symbol) a = RenameField (SProxy sym) a

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

instance decodeJsonWithCons_1
  :: ( Bind f
     , Cons s v () r
     , Cons ds dv () dr
     , IsSymbol s
     , IsSymbol ds
     , RowToList r l
     , RowToList dr dl
     , Status f
     -- , TypeEquals dv (Json -> a -> f (Tuple v (SProxy s)))
     --, TypeEquals dv (Json -> a -> Tuple (SProxy s) (f v))
     , TypeEquals dv (Json -> a -> RenameField s (f v))
     --, TypeEquals dv (Json -> a -> Record resultRow)
     --, Cons s (f v) () resultRow
     --, RowToList resultRow (Cons s (f v) Nil)
     --, ListToRow (Cons s (f v) Nil) resultRow
     )
  => DecodeJsonWith f (Cons ds dv Nil) dr (Cons s v Nil) r a
  where
  decodeJsonWith _ _ decoderRecord object x = do
    let
      oldSProxy :: SProxy ds
      oldSProxy = SProxy

      newSProxy :: SProxy s
      newSProxy = SProxy

      oldFieldName :: String
      oldFieldName = reflectSymbol oldSProxy

      -- decoder :: Json -> a -> f (Tuple v (SProxy s))
      --decoder :: Json -> a -> Tuple (SProxy s) (f v)
      decoder :: Json -> a -> RenameField s (f v)
      decoder = to $ get oldSProxy decoderRecord
      --decoder :: Json -> a -> resultRow
      --decoder :: Json -> a -> f v
      --decoder = get newSProxy $ to $ get oldSProxy decoderRecord

    case lookup oldFieldName object of
      Just jsonVal -> do
        --let Tuple _ fVal = decoder jsonVal x
        let RenameField _ fVal = decoder jsonVal x
        val <- fVal
        --val <- decoder jsonVal x
        report $ singleton newSProxy val
      Nothing ->
        reportError $ getMissingFieldErrorMessage oldFieldName

instance decodeJsonWithCons
  :: ( Bind f
     , SameSize1 dl r a
     , SameSize1 dl' r' a
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
     -- , TypeEquals dv (Json -> a -> f (Tuple v (SProxy s)))
     --, TypeEquals dv (Json -> a -> Tuple (SProxy s) (f v))
     , TypeEquals dv (Json -> a -> RenameField s (f v))
     --, TypeEquals dv (Json -> a -> Record resultRow)
     --, Cons s (f v) () resultRow
     --, RowToList resultRow (Cons s (f v) Nil)
     --, ListToRow (Cons s (f v) Nil) resultRow
     )
  => DecodeJsonWith f (Cons ds dv (Cons ds' dv' dl'')) dr (Cons s v (Cons s' v' l'')) r a
  where
  decodeJsonWith _ _ decoderRecord object x = do
    let
      oldSProxy :: SProxy ds
      oldSProxy = SProxy

      newSProxy :: SProxy s
      newSProxy = SProxy

      oldFieldName :: String
      oldFieldName = reflectSymbol oldSProxy

      -- decoder :: Json -> a -> f (Tuple v (SProxy s))
      --decoder :: Json -> a -> Tuple (SProxy s) (f v)
      decoder :: Json -> a -> RenameField s (f v)
      --decoder :: Json -> a -> f v
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
        let RenameField _ fVal = decoder jsonVal x
        val <- fVal
        report $ insert newSProxy val rest
      Nothing ->
        reportError $ getMissingFieldErrorMessage oldFieldName
