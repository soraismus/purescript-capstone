module Data.SameValues
  ( class SameValues
  ) where

import Type.Row (class Cons, Cons, Nil, kind RowList)

class SameValues (l :: RowList) (r :: # Type) | l -> r

-- instance sameValuesNil :: SameValues Nil ()
instance sameValuesNil :: SameValues Nil r

instance sameValuesCons
  :: ( Cons s0 v r' r
     , SameValues l' r'
     )
  => SameValues (Cons s1 v l') r
