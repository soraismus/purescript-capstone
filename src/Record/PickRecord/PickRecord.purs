module Record.Extra.ContractRecord.ContractRecord
  ( contractRecord
  ) where

import Record.Extra.ContractRecord.GContractRecord
  ( class GContractRecord
  , gContractRecord
  )
import Type.Row (class RowToList, class Union)
import Type.Row (RLProxy(RLProxy)) as TypeRow

contractRecord
  :: forall f l0 l1 l2 p r0 r1 r2
   . GContractRecord p f l0 r0 l1 r1 l2 r2
  => RowToList r0 l0
  => RowToList r1 l1
  => RowToList r2 l2
  => Union r2 r0 r1
  => p (f r1) (f r2)
contractRecord =
  gContractRecord
    (TypeRow.RLProxy :: TypeRow.RLProxy l0)
    (TypeRow.RLProxy :: TypeRow.RLProxy l1)
    (TypeRow.RLProxy :: TypeRow.RLProxy l2)
