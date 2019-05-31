module Data.Argonaut.Decode.Record.Cross.Class2
  ( class DecodeJsonWith
  , decodeJsonWith
  ) where

import Prelude (class Bind, bind, identity, ($), (<<<))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Record.Utils (getMissingFieldErrorMessage)
import Data.Maybe (Maybe(Just, Nothing))
import Data.SameKeys1 (class SameKeys1)
import Data.Status (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Foreign.Object (Object, lookup)
import Record (get, insert)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Equality (class TypeEquals, to)
import Type.Row
  ( class Cons
  , class Lacks
  , class ListToRow
  , class Nub
  , class RowToList
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
  (r1 :: # Type)          -- no longer necessary
  a
  (lsrc :: RowList)
  (rsrc :: # Type)
  (runion :: # Type)
  | l0 -> r0 r1 a
  , lsrc -> rsrc
  , l0 lsrc -> runion
  where
  decodeJsonWith
    :: RLProxy l0
    -> RLProxy lsrc
    -> Record r0
    -> Object Json
    -> a
    -> f (Record rsrc -> Record runion)

instance decodeJsonWithNil
  :: Status f
  => DecodeJsonWith f Nil () () a             Nil () ()
  where
  decodeJsonWith _ _ _ _ _ = report identity

instance decodeJsonWithCons
  :: ( Bind f
     , Cons s v r' r
     , Cons s dv dr' dr
     , IsSymbol s
     , Lacks s r'
     , Lacks s dr'
     , RowToList r l
     , RowToList r' l'
     , RowToList dr dl
     , RowToList dr' dl'
     , SameKeys1 dl r a
     , SameKeys1 dl' r' a
     , Status f
     , TypeEquals dv (Json -> a -> f v)
     , DecodeJsonWith f dl' dr' r' a           lsrc rsrc runion'

     , Nub runion runion
     , Union rsrc r1 runion
     , RowToList runion (Cons s v lunion')
     , ListToRow lunion' runion'
     , Lacks s runion'
     , Cons s v runion' runion
     )
  => DecodeJsonWith f (Cons s dv dl') dr r a        lsrc rsrc runion
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
