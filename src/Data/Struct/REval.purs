module Data.Struct.REval
  ( class REval
  , reval
  ) where

import Control.ClosedMonoidal (class ClosedMonoidal, class Evaluable, eval)
import Record.Builder (Builder)
import Record.Builder (build) as Builder
import Type.Row (kind RowList)
import Type.Row (RLProxy) as TypeRow

class REval
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  reval
    :: TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> p (f r0) (f r1)
    -> f r0
    -> f r1

instance revalBuilder :: REval Builder Record l0 r0 l1 r1 where
  reval _ _ = Builder.build

else instance revalRecord
  :: ( ClosedMonoidal p
     , Evaluable p (f r0) (f r1)
     )
  => REval p f l0 r0 l1 r1 where
  reval _ _ = eval
