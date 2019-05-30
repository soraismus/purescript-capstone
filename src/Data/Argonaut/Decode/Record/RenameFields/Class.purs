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

class HasSymbol2_
  (l :: RowList)
  (s :: Symbol)
  (b :: Boolean)
  (continue :: Boolean)
  | l s -> b
instance hasSymbol2_False :: HasSymbol2_ l s False False
instance hasSymbol2_Nil :: HasSymbol2_ Nil s False True
instance hasSymbol2_Cons
  :: ( HasSymbol2_ l' s1 output' uneq
     , Equals s s1 eq
     , Not eq uneq
     , If eq
         (BProxy True)
         (BProxy output')
         (BProxy output)
     )
  => HasSymbol2_ (Cons s0 (f s) l') s1 output True

class RenameFields_
  (l0 :: RowList)
  (l1 :: RowList)
  (l2 :: RowList)
  (continue :: Boolean)
  | l0 l1 -> l2
  , l0 l2 -> l1
--   , l1 l2 -> l0
instance renameFields_False :: RenameFields_ l0 l1 l2 False
instance renameFields_Nil :: RenameFields_ Nil l l True
-- instance renameFields_NilNil :: RenameFields_ Nil Nil Nil True
-- instance renameFields_NilCons
--   :: RenameFields_ Nil (Cons s1 v1 l1') (Cons s2 v2 l2') True

-- instance renameFields_Cons
--   :: ( Equals sa s1 eq
--      , Not eq uneq
--      , If eq
--           (RLProxy (Cons sb v l2_ifEq'))
--           (RLProxy l2_ifUneq)
--           (RLProxy (Cons s2 v l2'))
--      , HasSymbol_ l0' s1 has uneq
--      , Not has lacks
--      , And uneq has uneqAndHas
--      , And uneq lacks uneqAndLacks
--      , If has
--           (RLProxy l2_ifHas)
--           (RLProxy (Cons s1 v l2_ifLacks'))
--           (RLProxy l2_ifUneq)
--      , RenameFields_ l0'                  l1'             l2_ifEq'    eq
--      , RenameFields_ l0'                  (Cons s1 v l1') lx          uneqAndHas
--      , RenameFields_ (Cons sa (f sb) Nil) lx              l2_ifHas    uneqAndHas
--      , RenameFields_ (Cons sa (f sb) l0') l1'             l2_ifLacks' uneqAndLacks
--      )
--   => RenameFields_
--         (Cons sa (f sb) l0')
--         (Cons s1 v l1')
--         (Cons s2 v l2')
--         True

instance renameFields_Cons
  :: ( Equals sb s2 eq
     , Not eq uneq
     , If eq
          (RLProxy (Cons sa v l1_ifEq'))
          (RLProxy l1_ifUneq)
          (RLProxy (Cons s1 v l1'))
     , HasSymbol2_ l0' s2 has uneq
     , Not has lacks
     , And uneq has uneqAndHas
     , And uneq lacks uneqAndLacks
     , If has
          (RLProxy l1_ifHas)
          (RLProxy (Cons s2 v l1_ifLacks'))
          (RLProxy l1_ifUneq)
     , RenameFields_ l0'                  l1_ifEq'    l2'             eq
     , RenameFields_ l0'                  lx          (Cons s2 v l2') uneqAndHas
     , RenameFields_ (Cons sa (f sb) Nil) l1_ifHas    lx              uneqAndHas
     , RenameFields_ (Cons sa (f sb) l0') l1_ifLacks' l2'             uneqAndLacks
     )
  => RenameFields_
        (Cons sa (f sb) l0')
        (Cons s1 v l1')
        (Cons s2 v l2')
        True

-- instance renameFields_Cons
--   :: ( RenameFields_ l0' l1' l2_ifEq' eq1
--      , RenameFields_ (Cons sa0 (f sb) Nil) l0' uneq1_eq2_has1_lx uneq1_has1
--      , RenameFields_ l0' (Cons sa1 v1 uneq1_eq2_has1_lx) uneq1_eq2_has1_l2' uneq1_has1
--      , RenameFields_ (Cons sa0 (f sb) l0') l1' l2_ifLacks' uneq1_lacks1
--      , And uneq1 has1 uneq1_has1
--      , And uneq1 lacks1 uneq1_lacks1
--      , Not has1 lacks1
--      , HasSymbol_ l0' sa1 has1 uneq1
--      , If has1
--           (RLProxy uneq1_eq2_has1_l2'
--           (RLProxy (Cons sa1 v1 l2_ifLacks'))
--           (RLProxy l2_ifUneq')
--
--
--      , Equals sa0 sa1 eq1
--      , Equals sa0 sa2 eq2
--      , Not eq1 uneq1
--      , Not eq2 uneq2
--      , And eq1   eq2   eq1_eq2
--      , And eq1   uneq2 eq1_uneq2
--      , And uneq1 eq2   uneq1_eq2
--      , And uneq1 uneq2 uneq1_uneq2
--
--      -- 0. eq1 and eq2
--      , If eq1_eq2
--             (RLProxy (Cons sb v1 l2'))
--             (RLProxy l2_ifNotBothEq')
--             (RLProxy (Cons sa2 v2 l2'))
--
--      -- 0. eq1 and eq2
--      , RenameFields_ l0' l1' l2' eq1_eq2
--
--      -- 1. uneq1 and eq2
--      , HasSymbol_ l0' sa1 has1 uneq1_eq2
--      , Not has1 lacks1
--      , And uneq1_eq2 has1 uneq1_eq2_has1
--      , And uneq1_eq2 lacks1 uneq1_eq2_lacks1
--      , If has1
--           (RLProxy uneq1_eq2_has1_l2'
--           (RLProxy (Cons sa1 v1 l2_ifLacks'))
--           (RLProxy l2_ifUneq')
--      , RenameFields_ (Cons sa0 (f sb) Nil) l0' uneq1_eq2_has1_lx uneq1_eq2_has1
--      , RenameFields_ l0' (Cons sa1 v1 uneq1_eq2_has1_lx) uneq1_eq2_has1_l2' uneq1_eq2_has1
--
--      -- 2. eq1 and uneq2
--      -- 3. uneq1 and uneq2
--
--      )
--   => RenameFields_
--         (Cons sa0 (f sb) l0')
--         (Cons sa1 v1 l1')
--         (Cons sa2 v2 l2')
--         True

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
  => ListToRow l1 r1
  => RowToList r2 l2
  => f r0
  -> Record r1
  -> Record r2
renameFields nameChanges record =
    runFn2 renameFields' nameChanges' record
  where
  nameChanges' = fromFoldable $ reify' nameChanges

foreign import renameFields'
  :: forall r0 r1. Fn2 (Array (Tuple String String)) (Record r0) (Record r1)
