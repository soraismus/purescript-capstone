module Data.Argonaut.Decode.Record.Lazy.Class
  ( class GDecodeJson
  , gDecodeJson
  ) where

import Prelude (bind, identity, ($), (<<<))

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
  (r2 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l0 l1 -> r2
  where
  gDecodeJson
    :: RLProxy l0
    -> RLProxy l1
    -> Object Json
    -> Either String (Record r1 -> Record r2)

instance gDecodeJson_Nil :: GDecodeJson Nil () l r r where
  gDecodeJson _ _ _ = report identity

instance gDecodeJson_Cons
  :: ( Cons s v r0' r0
     , Cons s v r2' r2
     , D.DecodeJson v
     , GDecodeJson l0' r0' l1 r1 r2'
     , IsSymbol s
     , Lacks s r1
     , Lacks s r2'
     , Union r0 r1 r2
     )
  => GDecodeJson (Cons s v l0') r0 l1 r1 r2
  where
  gDecodeJson _ _ object = do
    let
      sProxy :: SProxy s
      sProxy = SProxy

      fieldName :: String
      fieldName = reflectSymbol sProxy

    doRest <-
      gDecodeJson
        (RLProxy :: RLProxy l0')
        (RLProxy :: RLProxy l1)
        object

    case lookup fieldName object of
      Just jsonVal -> do
        val <- D.decodeJson jsonVal
        report $ insert sProxy val <<< doRest
      Nothing ->
        reportError $ getMissingFieldErrorMessage fieldName
