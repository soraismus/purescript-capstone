module Data.Argonaut.Decode.Cases
  ( class Cases
  ) where

import Type.Row (class Cons, Cons, Nil, kind RowList)

class Cases
  (l :: RowList)
  (r :: # Type)
  | l -> r

instance casesNil :: Cases Nil ()

instance casesCons
  :: ( Cons s v r' r
     , Cases l' r'
     )
  => Cases (Cons s tv l') r
