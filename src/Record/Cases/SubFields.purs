module Record.Extra.SubFields
  ( class SubFields
  ) where

import Type.Row (class Cons, Cons, Nil, kind RowList)

class SubFields (l :: RowList) (r :: # Type) | l -> r

instance subFieldsNil :: SubFields Nil r

instance subFieldsCons_one
  :: Cons s v r' r
  => SubFields (Cons s v Nil) r

instance subFieldsCons_many
  :: ( Cons s0 v0 r' r
     , SubFields (Cons s1 v1 l'') r'
     )
  => SubFields (Cons s0 v0 (Cons s1 v1 l'')) r
