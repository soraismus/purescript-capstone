module Data.SameKeys
  ( class SameKeys
  ) where

import Type.Row (class Cons, Cons, Nil, kind RowList)

class SameKeys (l :: RowList) (r :: # Type) | l -> r

-- instance sameKeysNil :: SameKeys Nil ()
instance sameKeysNil :: SameKeys Nil r

instance sameKeysCons
  :: ( Cons s v0 r' r
     , SameKeys l' r'
     )
  => SameKeys (Cons s v1 l') r
