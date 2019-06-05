module Data.Struct.RGet
  ( class RGet
  , rget
  ) where

import Prelude (Ordering, const, eq, identity, pure, ($), (<<<))

import Control.Alternative (class Alternative)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (class VariantEqs, class VariantMatchCases, Variant)
import Data.Variant (contract, expand, inj, match, on, onMatch, prj) as Variant
import Data.Variant.Internal (class Contractable, class VariantTags)
import Record (get) as Record
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

class RGet
  (f :: # Type -> Type)
  (g :: Symbol -> Type)
  (s :: Symbol)
  (l :: RowList)
  (r :: # Type)
  | l -> r
  where
  rget :: forall r' v. Cons s v r' r => TypeRow.RLProxy l -> g s -> f r -> v

instance rgetRecord :: IsSymbol s => RGet Record SProxy s l r where
  rget _ = Record.get
