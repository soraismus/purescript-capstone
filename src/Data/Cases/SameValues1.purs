module Data.SameValues1
  ( class SameValues1
  ) where

import Type.Row (kind RowList, Cons, Nil, class Cons)

class SameValues1 (l :: RowList) (r :: # Type) a | l -> r

-- instance sameValues1Nil :: SameValues1 Nil () a
instance sameValues1Nil :: SameValues1 Nil r a

instance sameValues1Cons
  :: ( Cons s0 v r' r
     , SameValues1 l' r' a
     )
  => SameValues1 (Cons s1 v l') r a
