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

data FieldName (s :: Symbol) = FieldName

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

class RenameFields
  (l0 :: RowList)
  (l1 :: RowList)
  (l2 :: RowList)
  | l0 l1 -> l2
  , l0 l2 -> l1
instance renameFieldsRenameFields_
  :: RenameFields_ l0 l1 l2 True
  => RenameFields l0 l1 l2

renameFields
  :: forall f l0 l1 l2 r0 r1 r2
   . Reify l0
  => RenameFields l0 l1 l2
  => RowToList r0 l0
  => RowToList r1 l1
  -- => RowToList r2 l2
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

-- Why the distinct kind `SPairList`? Why not just `RowList s0 (f s1) ...`?
-- Or instead a list of SPairs?
-- foreign import kind SPair
-- foreign import data SPair :: Symbol -> Symbol -> SPair
-- foreign import kind SPairList
-- foreign import data SPairCons :: Symbol -> Symbol -> SPairList -> SPairList
-- foreign import data SPairNil :: SPairList
-- data SPairListProxy (a :: SPairList) = SPairListProxy
-- class Touches (l :: SPairList) (s :: Symbol) (b :: Boolean) | l s -> b
-- instance touchesNil :: Touches SPairNil s False
-- instance touchesCons
--   :: ( Touches l' s output'
--      , Symbol.Equals sa s eq
--      , Boolean.If eq
--          (BProxy True)
--          (BProxy output')
--          (BProxy output)
--      )
--   => Touches (SPairCons sa sb l') s output
-- class Touches_
--   (l :: SPairList)
--   (s :: Symbol)
--   (b :: Boolean)
--   (continue :: Boolean)
--   | l s -> b
-- instance touches_False :: Touches l s True False
-- instance touches_Nil :: Touches SPairNil s False True
-- instance touches_Cons
--   :: ( Touches_ l' s output' uneq
--      , Symbol.Equals sa s eq
--      , Not eq uneq
--      , Boolean.If eq
--          (BProxy True)
--          (BProxy output')
--          (BProxy output)
--      )
--   => Touches_ (SPairCons sa sb l') s output True
-- class SeparateSPairList
--   (a :: SPairList)
--   (b :: SList)
--   (c :: SList)
--   | a -> b c
--   , b c -> a
-- instance separateSPairNil :: SeparateSPairList SPairNil SNil SNil
-- instance separateSPairCons
--   :: SeparateSPairList a b c
--   => SeparateSPairList (SPairCons s0 s1 a) (SCons s0 b) (SCons s1 c)
-- class SPairListToRowList (a :: SPairList) (b :: RowList) | a -> b, b -> a
-- instance sPairListToRowListNil :: SPairListToRowList SPairNil Nil
-- instance sPairListToRowListCons
--   :: SPairListToRowList a b
--   => SPairListToRowList (SPairCons s0 s1 a) (Cons s0 (f s1) b)
-- class ReifySPairList (a :: SPairList) where
--   reify :: SPairListProxy a -> List (Tuple String String)
-- instance reifySPairNil :: ReifySPairList SPairNil where
--   reify = mempty
-- instance reifySPairCons
--   :: ( IsSymbol s0
--      , IsSymbol s1
--      , ReifySPairList l'
--      )
--   => ReifySPairList (SPairCons s0 s1 l')
--   where
--   reify _ = (Tuple string0 string1) : rest
--     where
--     string0 = reflectSymbol (SProxy :: SProxy s0)
--     string1 = reflectSymbol (SProxy :: SProxy s1)
--     rest = reify (SPairListProxy :: SPairListProxy l')
-- reify'
--   :: forall a f l r
--    . RowToList r l
--   => SPairListToRowList a l
--   => f r
--   -> List (Tuple String String)
-- reify' _ = reify (SPairListProxy a)
-- renameFields
--   :: dl dr r0 r1
--    . MhKeys dl
--   => RenameFields
--   => RowToList dr dl
--   -> Record r0
--   -> f dr
--   -> Record r1
-- renameFields record newFieldNames = runFn2 renameFields' record pairs
--   where
--   pairs = fromFoldable $ keys newFieldNames
-- class RenameFields_
--   (l0 :: SPairList)
--   (l1 :: RowList)
--   (l2 :: RowList)
--   (continue :: Boolean)
--   | l0 l1 -> l2
--   , l0 l2 -> l1
-- instance renameFields_False :: RenameFields_ l0 l1 l2 False
-- instance renameFields_Nil :: RenameFields_ SPairNil l l True
-- instance renameFields_Cons
--   :: ( RenameFields_ l0' l1' l2_ifTrue' eq
--      , RenameFields_ (SPairCons sa sb SPairNil) l0' lx uneqAndTouched
--      , RenameFields_ l0' (Cons s v lx) l2_ifFalse_0' uneqAndTouched
--      , RenameFields_ (SPairCons sa sb l0') l1' l2_ifFalse_1' uneqAndUntouched
--      , And uneq touched uneqAndTouched
--      , And uneq untouched uneqAndUntouched
--      , Not touched untouched
--      , Touches_ l0' s touched uneq
--      , Boolean.If touched
--           (RLProxy l2_ifFalse_0')
--           (RLProxy (Cons s v l2_ifFalse_1'))
--           (RLProxy l2_ifFalse')
--      , Not eq uneq
--      , Symbol.Equals sa s eq
--      , Boolean.If eq
--           (RLProxy (Cons sb v l2_ifTrue'))
--           (RLProxy l2_ifFalse')
--           (RLProxy l2)
--      )
--   => RenameFields_
--         (SPairCons sa sb l0')
--         (Cons s v l1')
--         l2
--         True

-- instance renameFieldsCons
--   :: ( Cons origS v r0' r0
--      , Cons origS (SProxy newS) dr' dr
--      , Cons newS  v             r2' r2
--      , IsSymbol origS
--      , IsSymbol newS
--      , Lacks    origS r0'
--      , Lacks    origS dr'
--      , Lacks    newS  r2'
--      , RenameFields l0' r0' dl' dr' l2' r2'
--      , RowToList  r0  l0
--      , RowToList  dr  dl
--      , RowToList  r2  l2
--      , SameKeys   dl r0
--      , SameSize   l0 l1
--      -- , SameValues l0 l1
--      )
--   => RenameFields
--         (Cons origS v l0')
--         r0
--         (Cons origS (SProxy newS) dl')
--         dr
--         (Cons newS v l2')
--         r2

-- class RenameFields
--   (l0 :: RowList)
--   (r0 :: # Type)
--   (l1 :: RowList)
--   (r1 :: # Type)
--   (l2 :: RowList)
--   (r2 :: # Type)
--   | l1 -> l0 r0 r1 l2 r2
-- --   where
-- --   renameFields
-- --     :: RLProxy l0
-- --     -> RLProxy l1
-- --     -> RLProxy l2
-- --     -> Record r1
-- --     -> Record r0
-- --     -> Record r2
--
-- instance renameFieldsNil :: RenameFields Nil () Nil () Nil ()
-- --   where
-- --   renameFields _ _ _ _ _ = {}
--
-- instance renameFieldsCons
--   :: ( Cons origS v r0' r0
--      --, Cons origS dv dr' dr
--      , Cons origS (SProxy newS) dr' dr
--      , Cons newS  v             r2' r2
--      , IsSymbol origS
--      , IsSymbol newS
--      , Lacks    origS r0'
--      , Lacks    origS dr'
--      , Lacks    newS  r2'
--      , RenameFields l0' r0' dl' dr' l2' r2'
--      , RowToList  r0  l0
--      , RowToList  r0' l0'
--      , RowToList  dr  dl
--      , RowToList  dr' dl'
--      , RowToList  r2  l2
--      , RowToList  r2' l2'
--      , SameKeys   l0  dr
--      , SameKeys   l0' dr'
--      , SameSize   dl  l2
--      , SameSize   dl' l2'
-- -- --      , SameValues l0  l2
-- -- --      , SameValues l0' l2'
-- --      --, TypeEquals dv (SProxy newS)
--      )
--   => RenameFields
--         (Cons origS v l0')
--         r0
--         --(Cons origS dv dl')
--         (Cons origS (SProxy newS) dl')
--         dr
--         (Cons newS v l2')
--         r2
-- --   where
-- --   renameFields _ _ _ decoderRecord origRecord =
-- --     let
-- --       origSProxy :: SProxy origS
-- --       origSProxy = SProxy
-- --
-- --       newSProxy :: SProxy newS
-- --       newSProxy = SProxy
-- --
-- --       -- To prevent unnecessary creation of intermediate records,
-- --       -- coercion is used rather than calling `Record.delete sProxy`
-- --       -- to induce the next expected type.
-- --       decoderRecord' :: Record dr'
-- --       decoderRecord' = unsafeCoerce decoderRecord
-- --
-- --       origRecord' :: Record r0'
-- --       origRecord' = unsafeCoerce origRecord
-- --
-- --       newRecord' :: Record r2'
-- --       newRecord' =
-- --         renameFields
-- --           (RLProxy :: RLProxy l0')
-- --           (RLProxy :: RLProxy dl')
-- --           (RLProxy :: RLProxy l2')
-- --           decoderRecord'
-- --           origRecord'
-- --
-- --     in
-- --       insert newSProxy (get origSProxy origRecord) newRecord'
