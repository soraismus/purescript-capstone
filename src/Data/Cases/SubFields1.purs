module Data.SubFields1
  ( class SubFields1
  ) where

import Type.Row (class Cons, Cons, Nil, kind RowList)

class SubFields1 (l :: RowList) (r :: # Type) a | l -> r

instance subFields1Nil :: SubFields1 Nil r a

instance subFields1Cons
  :: ( Cons s v r' r
     , SubFields1 l' r a
     )
  => SubFields1 (Cons s v l') r a
