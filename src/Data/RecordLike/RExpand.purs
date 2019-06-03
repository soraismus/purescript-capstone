module Data.RecordLike.RExpand
  ( class RExpand
  , rexpand
  ) where

import Prelude (Ordering, const, eq, identity, pure, ($), (<<<))

import Control.Alternative (class Alternative)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (class VariantEqs, class VariantMatchCases, Variant)
import Data.Variant (expand) as Variant
import Data.Variant.Internal (class Contractable, class VariantTags)
import Record.Builder (Builder)
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

class RExpand
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rexpand
    :: forall r
     . Union r0 r r1
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> p (f r0) (f r1)

instance rexpandRProxy :: RExpand Function RProxy l0 r0 l1 r1 where
  rexpand _ _ _ = RProxy

instance rexpandVariant :: RExpand Function Variant l0 r0 l1 r1 where
  rexpand _ _ = Variant.expand

