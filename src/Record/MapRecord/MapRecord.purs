module Record.Extra.MapRecord.MapRecord
  ( mapRecord
  ) where

import Record.Extra.MapRecord.GMapRecord (class GMapRecord, gMapRecord)
import Type.Row (class RowToList, kind RowList)
import Type.Row (RLProxy(RLProxy)) as TypeRow

mapRecord
  :: forall f l0 l1 p r0 r1 r2
   . GMapRecord p f l0 r0 l1 r1 r2
  => RowToList r0 l0
  => RowToList r1 l1
  => Record r0
  -> p (f r1) (f r2)
mapRecord =
  gMapRecord
    (TypeRow.RLProxy :: TypeRow.RLProxy l0)
    (TypeRow.RLProxy :: TypeRow.RLProxy l1)
