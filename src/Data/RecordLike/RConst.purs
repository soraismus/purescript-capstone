module Data.RecordLike.RConst
  ( class RConst
  , rconst
  ) where

import Control.Applicative (class Applicative)
import Control.ClosedMonoidal (class ClosedMonoidal, const, (<<<))
import Record.Builder (Builder)
import Type.Row (kind RowList)
import Type.Row (RLProxy) as TypeRow
import Unsafe.Coerce (unsafeCoerce)

class RConst
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rconst
    :: TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> f r0
    -> p (f r1) (f r0)

instance rconstBuilder :: RConst Builder Record l0 r0 l1 r1 where
  rconst _ _ = mkConstBuilder
    where
    mkBuilder
      :: forall r r'
       . (Record r -> Record r')
      -> Builder (Record r) (Record r')
    mkBuilder = unsafeCoerce
    mkConstBuilder :: forall r r'. Record r -> Builder (Record r') (Record r)
    mkConstBuilder = mkBuilder <<< const

else instance rconstRecord
  :: ( Applicative (p (f r1))
     , ClosedMonoidal p
     )
  => RConst p f l0 r0 l1 r1
  where
  rconst _ _ = const
