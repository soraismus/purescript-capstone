module Data.RecordLike.RContract
  ( class RContract
  , rcontract
  ) where

import Prelude (pure, ($))

import Control.Alternative (class Alternative)
import Data.Variant (Variant)
import Data.Variant (contract) as Variant
import Data.Variant.Internal (class Contractable)
import Record.Extra (class Keys, pick) as RecordExtra
import Type.Row (class RowToList, class Union, RProxy(RProxy), kind RowList)
import Type.Row (RLProxy) as TypeRow

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
    :: forall h r
     . Alternative h
    => Union r1 r r0
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> p (f r0) (h (f r1))

instance rcontractRecord
  :: ( RecordExtra.Keys l1
     , RowToList r1 l1
     )
  => RContract Function Record l0 r0 l1 r1
  where
  rcontract _ _ record = pure $ RecordExtra.pick record

instance rcontractRProxy :: RContract Function RProxy l0 r0 l1 r1 where
  rcontract _ _ _ = pure RProxy

instance rcontractVariant
  :: Contractable r0 r1
  => RContract Function Variant l0 r0 l1 r1
  where
  rcontract _ _ = Variant.contract
