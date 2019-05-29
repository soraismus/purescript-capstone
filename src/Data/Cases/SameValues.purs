module Data.SameValues where

import Type.Data.Boolean (class If, False, True, kind Boolean)
import Type.Data.RowList (RLProxy) -- Argonaut dependency
import Type.Equality (class TypeEquals)
import Type.Row (Cons, Nil, kind RowList)

class HasValue (l :: RowList) a
instance hasValueCons_one :: HasValue (Cons s a Nil) a
instance hasValueCons_many_match
  :: HasValue (Cons s0 a (Cons s1 a1 l'')) a
else instance hasValueCons_many_nonMatch
  :: HasValue (Cons s1 a1 l'') a2
  => HasValue (Cons s0 a0 (Cons s1 a1 l'')) a2

class TypeEqualsOrNot lhs rhs (out :: Boolean) | lhs rhs -> out
instance typeEqualsOrNotTrue :: TypeEquals lhs rhs => TypeEqualsOrNot lhs rhs True
else instance typeEqualsOrNotFalse :: TypeEqualsOrNot lhs rhs False

class RowListRemoveValue
  (value  :: Type   )
  (input  :: RowList)
  (output :: RowList)
  | value input -> output
instance rowListRemoveValueNil :: RowListRemoveValue value Nil Nil
instance rowListRemoveValueCons
  :: ( RowListRemoveValue value0 input' output'
     , TypeEqualsOrNot value0 value1 eq
     , If eq
         (RLProxy input')
         (RLProxy (Cons s value1 output'))
         (RLProxy output)
     )
  => RowListRemoveValue value0 (Cons s value1 input') output

class SameValues (l0 :: RowList) (l1 :: RowList)
instance sameValuesNil :: SameValues Nil l
instance sameValuesCons_one
  :: HasValue l v
  => SameValues (Cons s v Nil) l
instance sameValuesCons_many
  :: ( HasValue l1 va
     , RowListRemoveValue va l1 l1'
     , SameValues (Cons sb vb l0'') l1'
     )
  => SameValues (Cons sa va (Cons sb vb l0'')) l1
