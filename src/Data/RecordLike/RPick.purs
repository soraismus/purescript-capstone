module Data.RecordLike.RPick
  ( class RPick
  , rpick
  ) where

-- import Record.Builder (Builder)
import Record.Extra (class Keys, pick) as RecordExtra
-- import Record.Extra.PickRecord (pickRecord) as PickRecord
import Type.Row (class RowToList, class Union, RProxy(RProxy), kind RowList)
import Type.Row (RLProxy) as TypeRow

class RPick
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rpick
    :: forall r
     . Union r1 r r0
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> p (f r0) (f r1)

-- import Record.Extra.PickRecord.GPickRecord (class GPickRecord, gPickRecord)
-- instance rpickBuilder
--   => GPickRecord Builder Record l0 r0 l1 r1 l2 r2
--   :: RPick Builder Record l0 r0 l1 r1 where
--   rpick _ _ =
--     PickRecord.gPickRecord
--       (TypeRow.RLProxy :: TypeRow.RLProxy l0)
--       (TypeRow.RLProxy :: TypeRow.RLProxy l1)
--       (TypeRow.RLProxy :: TypeRow.RLProxy l2)

instance rpickRecord
  :: ( RecordExtra.Keys l1
     , RowToList r1 l1
     )
  => RPick Function Record l0 r0 l1 r1 where
  rpick _ _ = RecordExtra.pick

instance rpickRProxy :: RPick Function RProxy l0 r0 l1 r1 where
  rpick _ _ _ = RProxy
