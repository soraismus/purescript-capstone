module Data.Argonaut.Decode.Record.Lazy.Utils
  ( decodeJson
  ) where

import Prelude (($))

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Record.Lazy.Class
  ( class GDecodeJson
  , gDecodeJson
  ) as D
import Data.Argonaut.Decode.Record.Utils (reportJson)
import Data.Either (Either)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row (class RowToList, class Union)

-- decodeJson
--   :: forall f l0 r0 l1 r1 r2
--    . D.GDecodeJson l0 r0 l1 r1 r2
--   => RowToList r0 l0
--   => RowToList r1 l1
--   => Union r0 r1 r2
--   => f r0
--   -> Json
--   -> Either String (Record r2)
-- decodeJson _ = decodeJson' (RProxy ::
--   reportJson
--     $ D.gDecodeJson
--         (RLProxy :: RLProxy l0)
--         (RLProxy :: RLProxy l1)

decodeJson
  :: forall f l0 r0 l1 r1 r2
   . D.GDecodeJson l0 r0 l1 r1 r2
  => RowToList r0 l0
  => RowToList r1 l1
  => Union r0 r1 r2
  => f r0
  -> Json
  -> Either String (Record r1 -> Record r2)
decodeJson _ =
  reportJson
    $ D.gDecodeJson
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l1)
