module Data.SubFields
  ( class SubFields
  ) where

import Type.Row (class Cons, Cons, Nil, kind RowList)

class SubFields (l :: RowList) (r :: # Type) | l -> r

instance subFieldsNil :: SubFields Nil r

instance subFieldsCons
  :: ( Cons s v r' r
     , SubFields l' r
     )
  => SubFields (Cons s v l') r
