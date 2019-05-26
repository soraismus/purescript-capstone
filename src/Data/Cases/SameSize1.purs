module Data.SameSize1
  ( class SameSize1
  ) where

import Type.Row (kind RowList, Cons, Nil, class Cons)

class SameSize1
  (f :: Type -> Type)
  (l :: RowList)
  (r :: # Type)
  a
  | l -> r

instance sameSize1Nil :: SameSize1 f Nil () a

instance sameSize1Cons
  :: ( Cons s0 v0 r' r
     , SameSize1 f l' r' a
     )
  => SameSize1 f (Cons s1 v1 l') r a
