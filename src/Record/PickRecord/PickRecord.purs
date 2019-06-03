module Record.Extra.PickRecord.PickRecord
  ( pickRecord
  ) where

import Record.Extra.PickRecord.GPickRecord (class GPickRecord, gPickRecord)
import Type.Row (class RowToList)
import Type.Row (RLProxy(RLProxy)) as TypeRow

pickRecord
  :: forall f l0 l1 l2 p r0 r1 r2
   . GPickRecord p f l0 r0 l1 r1 l2 r2
  => RowToList r0 l0
  => RowToList r1 l1
  => RowToList r2 l2
  => p (f r1) (f r2)
pickRecord =
  gPickRecord
    (TypeRow.RLProxy :: TypeRow.RLProxy l0)
    (TypeRow.RLProxy :: TypeRow.RLProxy l1)
    (TypeRow.RLProxy :: TypeRow.RLProxy l2)
