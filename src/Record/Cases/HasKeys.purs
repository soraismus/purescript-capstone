module Record.Extra.HasKeys
  ( class HasKeys
  ) where

import Type.Row (class Cons, Cons, Nil, kind RowList)

class HasKeys (l :: RowList) (r :: # Type) | l -> r

instance sameKeysNil :: HasKeys Nil r

instance sameKeysCons_one
  :: Cons s0 v r' r
  => HasKeys (Cons s0 v0 Nil) r

instance sameKeysCons_many
  :: ( Cons s0 v r' r
     , HasKeys (Cons s1 v1 l'') r'
     )
  => HasKeys (Cons s0 v0 (Cons s1 v1 l'')) r
