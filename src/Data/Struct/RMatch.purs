module Data.Struct.RMatch
  ( class RMatch
  , rmatch
  ) where

import Prelude (Ordering, const, eq, identity, pure, ($), (<<<))

import Control.Alternative (class Alternative)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (class VariantEqs, class VariantMatchCases, Variant)
import Data.Variant ( match) as Variant
import Data.Variant.Internal (class Contractable, class VariantTags)
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

class RMatch
  (f  :: # Type -> Type)
  (g  :: # Type -> Type)
  (v  :: Type)
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
  rmatch
    :: Union r1 () r2
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> TypeRow.RLProxy l2
    -> f r0
    -> g r2
    -> v

instance rmatchVariant
  :: ( RowToList r0 l0
     , VariantMatchCases l0 r1 v
     )
  => RMatch Record Variant v l0 r0 l1 r1 l2 r2
  where
  rmatch _ _ _ = Variant.match
