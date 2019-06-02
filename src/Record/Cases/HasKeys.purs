module Record.Extra.HasKeys
  ( class HasKeys
  ) where

import Type.Row (class Cons, Cons, Nil, kind RowList)

class HasKeys (l :: RowList) (r :: # Type) | l -> r

instance hasKeys_Nil :: HasKeys Nil r

instance hasKeys_Cons
  :: ( Cons s0 v r' r
     , HasKeys l' r'
     )
  => HasKeys (Cons s0 v0 l') r
