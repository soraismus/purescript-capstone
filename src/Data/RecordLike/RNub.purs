module Data.RecordLike.RNub
  ( class RNub
  , rnub
  ) where

import Prelude (Ordering, const, eq, identity, pure, ($), (<<<))
import Control.Alternative (class Alternative)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Record (nub) as Record
import Record.Builder (Builder)
import Record.Builder (nub) as Builder
import Type.Row
  ( class Cons
  , class Lacks
  , class ListToRow
  , class Nub
  , class RowToList
  , class Union
  , Cons
  , Nil
  , RProxy(RProxy)
  , kind RowList
  )
import Type.Row (RLProxy) as TypeRow
import Unsafe.Coerce (unsafeCoerce)

class RNub
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rnub
    :: Nub r0 r1
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> p (f r0) (f r1)

instance rnubBuilder :: RNub Builder Record l0 r0 l1 r1 where
  rnub _ _ = Builder.nub

instance rnubRecord :: RNub Function Record l0 r0 l1 r1 where
  rnub _ _ = Record.nub

instance rnubRProxy :: RNub Function RProxy l0 r0 l1 r1 where
  rnub _ _ _ = RProxy

instance rnubVariant :: RNub Function Variant l0 r0 l1 r1 where
  rnub _ _ = unsafeCoerce