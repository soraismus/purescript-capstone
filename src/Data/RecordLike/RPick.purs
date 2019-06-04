module Data.RecordLike.RPick
  ( class RPick
  , rpick
  ) where

import Record.Builder (Builder)
import Record.Extra (class Keys, pick) as RecordExtra
import Record.Extra.PickRecord.GPickRecord (class GPickRecord)
import Record.Extra.PickRecord.GPickRecord (gPickRecord) as PickRecord
import Type.Row (class RowToList, class Union, RProxy(RProxy), kind RowList)
import Type.Row (RLProxy(RLProxy)) as TypeRow

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

instance rpickBuilder
  :: ( GPickRecord Builder Record l r l0 r0 l1 r1
     , RowToList r l
     , Union r1 r r0
     )
  => RPick Builder Record l0 r0 l1 r1
  where
  rpick =
    PickRecord.gPickRecord
      (TypeRow.RLProxy :: TypeRow.RLProxy l)

instance rpickRecord
  :: ( RecordExtra.Keys l1
     , RowToList r1 l1
     )
  => RPick Function Record l0 r0 l1 r1 where
  rpick _ _ = RecordExtra.pick

instance rpickRProxy :: RPick Function RProxy l0 r0 l1 r1 where
  rpick _ _ _ = RProxy
