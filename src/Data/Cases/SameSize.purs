module Data.SameSize
  ( class SameSize
  ) where

import Type.Row (kind RowList, Cons, Nil, class Cons)

class SameSize
  (f :: Type -> Type)
  (l :: RowList)
  (r :: # Type)
  | l -> r

instance sameSizeNil :: SameSize f Nil ()

instance sameSizeCons
  :: ( Cons s0 v0 r' r
     , SameSize f l' r'
     )
  => SameSize f (Cons s1 v1 l') r
