module Data.RecordLike.RProject
  ( class RProject
  , rproject
  ) where

import Prelude (Ordering, const, eq, identity, pure, ($), (<<<))
import Control.Alternative (class Alternative)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (class VariantEqs, class VariantMatchCases, Variant)
import Data.Variant (prj) as Variant
import Data.Variant.Internal (class Contractable, class VariantTags)
import Record
  ( class EqualFields
  , delete
  , disjointUnion
  , get
  , equal
  , insert
  , merge
  , modify
  , nub
  , rename
  , set
  , union
  ) as Record
import Record.Builder (Builder)
import Record.Builder
  ( delete
  , disjointUnion
  , insert
  , merge
  , modify
  , nub
  , rename
  , union
  ) as Builder
-- import Record.Extra.PickRecord (pickRecord) as PickRecord
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

class RProject
  (f  :: # Type -> Type)
  (g  :: Symbol -> Type)
  (s  :: Symbol)
  (l :: RowList)
  (r :: # Type)
  | l -> r
  where
  rproject
    :: forall h r' v
     . Alternative h
    => Cons s v r' r
    => TypeRow.RLProxy l
    -> g s
    -> f r
    -> h v

instance rprojectRecord :: IsSymbol s => RProject Record SProxy s l r where
  rproject _ s record = pure $ Record.get s record

instance rprojectVariant :: IsSymbol s => RProject Variant SProxy s l r where
  rproject _ = Variant.prj
