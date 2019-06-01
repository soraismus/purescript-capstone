module Data.Argonaut.Decode.Record.Lazy.Utils
  ( decodeJson
  , decodeJson'
  ) where

import Prelude (bind, pure, ($))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Record.Lazy.Class
  ( class GDecodeJson
  , gDecodeJson
  ) as D
import Data.Argonaut.Decode.Record.Utils (reportJson)
import Data.Either (Either)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row (class RowToList, class Union, Nil)

decodeJson
  :: forall l r
   . D.GDecodeJson l r Nil () l r
  => RowToList r l
  => RowToList r l
  => Union r () r
  => Json
  -> Either String (Record r)
decodeJson json = do
  f <- decodeJson' json
  pure $ f {}

decodeJson'
  :: forall l0 l1 l2 r0 r1 r2
   . D.GDecodeJson l0 r0 l1 r1 l2 r2
  => RowToList r1 l1
  => RowToList r2 l2
  => Union r0 r1 r2
  => Json
  -> Either String (Record r1 -> Record r2)
decodeJson' =
  reportJson
    $ D.gDecodeJson
        (RLProxy :: RLProxy l1)
        (RLProxy :: RLProxy l2)
