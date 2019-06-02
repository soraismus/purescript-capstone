module Record.Extra.SameSize
  ( class SameSize
  ) where

import Type.Row (kind RowList, Cons, Nil)

class SameSize (l0 :: RowList) (l1 :: RowList)

instance sameSizeNil :: SameSize Nil Nil

instance sameSizeCons
  :: SameSize l0' l1'
  => SameSize (Cons s0 v0 l0') (Cons s1 v1 l1')
