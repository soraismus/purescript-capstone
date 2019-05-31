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
  => DecodeJsonWith f Nil () ()             lsrc rsrc rsrc a
  where
  decodeJsonWith _ _ _ _ _ = report identity

instance decodeJsonWithCons
  :: ( Bind f
     , Cons s dv dr' dr
     , Cons s v rdiff' rdiff
     , Cons s v runion' runion
     , DecodeJsonWith f dl' dr' rdiff' lsrc rsrc runion' a
     , IsSymbol s
     , Status f
     , Lacks s rdiff'
     , Lacks s runion'
     , TypeEquals dv (Json -> a -> f v)
     , Union rdiff rsrc runion
     )
  => DecodeJsonWith f (Cons s dv dl') dr rdiff lsrc rsrc runion a
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
      decoderRecord' :: Record dr'
      decoderRecord' = unsafeCoerce decoderRecord

    doRest <-
      decodeJsonWith
        (RLProxy :: RLProxy dl')
        (RLProxy :: RLProxy lsrc)
        decoderRecord'
        object
        x

    case lookup fieldName object of
      Just jsonVal -> do
        val <- decoder jsonVal x
        report $ insert sProxy val <<< doRest
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName
