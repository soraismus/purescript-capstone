module Data.SameSize1
  ( class SameSize1
  ) where

import Type.Row (kind RowList, Cons, Nil, class Cons)

class SameSize1 (l :: RowList) (r :: # Type) a | l -> r

instance sameSize1Nil :: SameSize1 Nil () a

instance sameSize1Cons
  :: ( Cons s0 v0 r' r
     , SameSize1 l' r' a
     )
  => SameSize1 (Cons s1 v1 l') r a
