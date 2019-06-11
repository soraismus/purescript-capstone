module Data.Struct.RContract
  ( class RContract
  , rcontract
  ) where

import Record.Builder (Builder)
import Record.Extra (class Keys, contract) as RecordExtra
import Record.Extra.ContractRecord.GContractRecord (class GContractRecord)
import Record.Extra.ContractRecord.GContractRecord (gContractRecord) as ContractRecord
import Type.Row (class RowToList, class Union, RProxy(RProxy), kind RowList)
import Type.Row (RLProxy(RLProxy)) as TypeRow

class RContract
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rcontract
    :: forall r
     . Union r1 r r0
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> p (f r0) (f r1)

instance rcontractBuilder
  :: ( GContractRecord Builder Record l0 r0 l1 r1 l2 r2
     , RowToList r0 l0
     , Union r2 r0 r1
     )
  => RContract Builder Record l1 r1 l2 r2
  where
  rcontract =
    ContractRecord.gContractRecord
      (TypeRow.RLProxy :: TypeRow.RLProxy l0)

instance rcontractRecord
  :: ( RecordExtra.Keys l1
     , RowToList r1 l1
     )
  => RContract Function Record l0 r0 l1 r1 where
  rcontract _ _ = RecordExtra.pick

instance rcontractRProxy :: RContract Function RProxy l0 r0 l1 r1 where
  rcontract _ _ _ = RProxy
