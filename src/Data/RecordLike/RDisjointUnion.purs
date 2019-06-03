module Data.RecordLike.RDisjointUnion
  ( class RDisjointUnion
  , rdisjointUnion
  ) where

import Prelude (Ordering, const, eq, identity, pure, ($), (<<<))

import Control.Alternative (class Alternative)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (class VariantEqs, class VariantMatchCases, Variant)
import Data.Variant.Internal (class Contractable, class VariantTags)
import Record (disjointUnion) as Record
import Record.Builder (Builder)
import Record.Builder (disjointUnion) as Builder
-- import Record.Extra.PickRecord (pickRecord) as PickRecord
import Record.Extra.Utils (singleton) as RecordExtra
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

class RDisjointUnion
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l2 :: RowList)
  (r2 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l2 -> r2
  where
  rdisjointUnion
    :: Nub r2 r2
    => Union r0 r1 r2
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> TypeRow.RLProxy l2
    -> f r0
    -> p (f r1) (f r2)

instance rdisjointUnionBuilder
  :: RDisjointUnion Builder Record l0 r0 l1 r1 l2 r2
  where
  rdisjointUnion _ _ _ = Builder.disjointUnion

instance rdisjointUnionRecord
  :: RDisjointUnion Function Record l0 r0 l1 r1 l2 r2
  where
  rdisjointUnion _ _ _ = Record.disjointUnion

instance rdisjointUnionRProxy
  :: RDisjointUnion Function RProxy l0 r0 l1 r1 l2 r2
  where
  rdisjointUnion _ _ _ _ _= RProxy
