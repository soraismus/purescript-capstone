module Data.SameKeys
  ( class SameKeys
  ) where

import Type.Row (class Cons, Cons, Nil, kind RowList)

class SameKeys (l :: RowList) (r :: # Type) | l -> r

instance sameKeysNil :: SameKeys Nil r

instance sameKeysCons_one
  :: Cons s0 v r' r
  => SameKeys (Cons s0 v0 Nil) r

instance sameKeysCons_many
  :: ( Cons s0 v r' r
     , SameKeys (Cons s1 v1 l'') r'
     )
  => SameKeys (Cons s0 v0 (Cons s1 v1 l'')) r
