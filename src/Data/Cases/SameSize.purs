module Data.SameSize
  ( class SameSize
  ) where

import Type.Row (kind RowList, Cons, Nil, class Cons)

class SameSize (l :: RowList) (r :: # Type) | l -> r

instance sameSizeNil :: SameSize Nil ()

instance sameSizeCons
  :: ( Cons s0 v0 r' r
     , SameSize l' r'
     )
  => SameSize (Cons s1 v1 l') r
