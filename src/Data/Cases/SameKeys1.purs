module Data.SameKeys1
  ( class SameKeys1
  ) where

import Type.Row (kind RowList, Cons, Nil, class Cons)

class SameKeys1 (l :: RowList) (r :: # Type) a | l -> r

-- instance sameKeys1Nil :: SameKeys1 Nil () a
instance sameKeys1Nil :: SameKeys1 Nil r a

instance sameKeys1Cons
  :: ( Cons s v0 r' r
     , SameKeys1 l' r' a
     )
  => SameKeys1 (Cons s v1 l') r a
