module Data.RecordLike.REqual
  ( class REqual
  , requal
  ) where

import Prelude (Ordering, const, eq, identity, pure, ($), (<<<))

import Control.Alternative (class Alternative)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (class VariantEqs, class VariantMatchCases, Variant)
import Data.Variant (contract, expand, inj, match, on, onMatch, prj) as Variant
import Data.Variant.Internal (class Contractable, class VariantTags)
import Record (class EqualFields, equal) as Record
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

class REqual
  (f :: # Type -> Type)
  (l :: RowList)
  (r :: # Type)
  | l -> r
  where
  requal :: RowToList r l => TypeRow.RLProxy l -> f r -> f r -> Boolean

instance requalRecord :: Record.EqualFields l r => REqual Record l r where
  requal _ = Record.equal

instance requalRProxy :: REqual RProxy l r where
  requal _ _ _ = true

instance requalVariant
  :: ( VariantEqs l
     , VariantTags l
     )
  => REqual Variant l r where
  requal _ = eq
