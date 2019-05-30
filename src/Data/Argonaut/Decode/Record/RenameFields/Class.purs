module Data.Argonaut.Decode.Record.RenameFields.Class
  where

import Prelude

import Data.Array (fromFoldable)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.List (List, (:))
import Data.Tuple (Tuple(Tuple))
import Type.Data.Boolean
  ( class And
  , class If
  , class Not
  , BProxy
  , False
  , True
  , kind Boolean
  )
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Data.Symbol
  ( class Equals
  , class IsSymbol
  , SProxy(SProxy)
  , reflectSymbol
  )
import Type.Row (class ListToRow, class RowToList, Cons, Nil, kind RowList)

class HasSymbol (l :: RowList) (s :: Symbol) (b :: Boolean) | l s -> b
instance hasSymbol :: HasSymbol_ l s b True => HasSymbol l s b

class HasSymbol_
  (l :: RowList)
  (s :: Symbol)
  (b :: Boolean)
  (continue :: Boolean)
  | l s -> b
instance hasSymbol_False :: HasSymbol_ l s False False
instance hasSymbol_Nil :: HasSymbol_ Nil s False True
instance hasSymbol_Cons
  :: ( HasSymbol_ l' s1 output' uneq
     , Equals s0 s1 eq
     , Not eq uneq
     , If eq
         (BProxy True)
         (BProxy output')
         (BProxy output)
     )
  => HasSymbol_ (Cons s0 v l') s1 output True

class RenameFields_
  (l0 :: RowList)
  (l1 :: RowList)
  (l2 :: RowList)
  (continue :: Boolean)
  | l0 l1 -> l2
  , l0 l2 -> l1
  , l1 l2 -> l0
instance renameFields_False :: RenameFields_ l0 l1 l2 False
instance renameFields_Nil :: RenameFields_ Nil l l True
instance renameFields_Cons
  :: ( RenameFields_ l0' l1' l2_ifEq' eq
     , RenameFields_ (Cons sa (f sb) Nil) l0' lx uneqAndHas
     , RenameFields_ l0' (Cons s v lx) l2_ifHas' uneqAndHas
     , RenameFields_ (Cons sa (f sb) l0') l1' l2_ifLacks' uneqAndLacks
     , And uneq has uneqAndHas
     , And uneq lacks uneqAndLacks
     , Not has lacks
     , HasSymbol_ l0' s has uneq
     , If has
          (RLProxy l2_ifHas')
          (RLProxy (Cons s v l2_ifLacks'))
          (RLProxy l2_ifUneq')
     , Not eq uneq
     , Equals sa s eq
     , If eq
          (RLProxy (Cons sb v l2_ifEq'))
          (RLProxy l2_ifUneq')
          (RLProxy l2)
     )
  => RenameFields_
        (Cons sa (f sb) l0')
        (Cons s v l1')
        l2
        True

class RenameFields
  (l0 :: RowList)
  (l1 :: RowList)
  (l2 :: RowList)
  | l0 l1 -> l2
  , l0 l2 -> l1
instance renameFieldsRenameFields_
  :: RenameFields_ l0 l1 l2 True
  => RenameFields l0 l1 l2

class Reify (l :: RowList) where
  reify :: RLProxy l -> List (Tuple String String)
instance reifyNil :: Reify Nil where
  reify = mempty
instance reifyCons
  :: ( IsSymbol s0
     , IsSymbol s1
     , Reify l'
     )
  => Reify (Cons s0 (f s1) l')
  where
  reify _ = (Tuple string0 string1) : rest
    where
    string0 = reflectSymbol (SProxy :: SProxy s0)
    string1 = reflectSymbol (SProxy :: SProxy s1)
    rest = reify (RLProxy :: RLProxy l')
reify'
  :: forall f l r
   . Reify l
  => RowToList r l
  => f r
  -> List (Tuple String String)
reify' _ = reify (RLProxy :: RLProxy l)

renameFields
  :: forall f l0 l1 l2 r0 r1 r2
   . Reify l0
  => RenameFields l0 l1 l2
  => RowToList r0 l0
  => RowToList r1 l1
  => ListToRow l2 r2
  => f r0
  -> Record r1
  -> Record r2
renameFields nameChanges record =
    runFn2 renameFields' nameChanges' record
  where
  nameChanges' = fromFoldable $ reify' nameChanges

foreign import renameFields'
  :: forall r0 r1. Fn2 (Array (Tuple String String)) (Record r0) (Record r1)
