module Data.Argonaut.Decode.Cases1
  ( class Cases1
  ) where

import Type.Row (kind RowList, Cons, Nil, class Cons)

class Cases1
  (f :: Type -> Type)
  (l :: RowList)
  (r :: # Type)
  a
  | l -> r

instance cases1Nil :: Cases1 f Nil () a

instance cases1Cons
  :: ( Cons field v r' r
     , Cases1 f l' r' a
     )
  => Cases1 f (Cons field tv l') r a
