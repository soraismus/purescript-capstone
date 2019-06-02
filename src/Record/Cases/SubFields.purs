module Record.Extra.SubFields
  ( class SubFields
  ) where

import Type.Row (class Cons, Cons, Nil, kind RowList)

class SubFields (l :: RowList) (r :: # Type) | l -> r

instance subFields_Nil :: SubFields Nil r

instance subFields_Cons
  :: ( Cons s0 v0 r' r
     , SubFields l' r'
     )
  => SubFields (Cons s0 v0 l') r
