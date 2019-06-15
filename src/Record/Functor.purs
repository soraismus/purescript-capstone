module Record.Extra.Functor.MapRecord.GMapRecord
  ( class GFunctor
  , gfunctor
  )
  where

import Type.Data.Boolean
  ( class And
  , class If
  , class Not
  , class Or
  , BProxy
  , False
  , True
  , kind Boolean
  )
import Partial.Unsafe (unsafeCrashWith)

import Prelude (class Category, class Semigroupoid, identity, (<<<))

import Data.Functor (class Functor)
import Data.Struct (class RGet, class RModify, rget, rmodify)
import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Type.Row (class Cons, Cons, Nil, kind RowList)
import Type.Row (RLProxy(RLProxy)) as TypeRow
import Unsafe.Coerce (unsafeCoerce)

class Cut
  (l0 :: RowList)
  (l1 :: RowList)
  (l2 :: RowList)
  | l0 l1 -> l2

instance cutNil :: Cut Nil l Nil
instance cutConsCons
  :: ( Cut l0' l1 l2'
     , HasSymbol l1 s0 has
     , If has
          (RLProxy (Cons s0 v0 l2'))
          (RLProxy l2')
          (RLProxy l2)
     )
  => Cut (Cons s0 v0 l0') l1 l2

class GFunctor
  (p  :: Type -> Type -> Type)
  (f :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (r2 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l0 l1 -> r2
  where
  gfunctor
    :: TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> Record r0
    -> p (f r1) (f r2)

-- RECYCLE FROM module Record.Extra.MapRecord.GMapRecord
instance gfunctorNil :: Category p => GFunctor p f Nil () l r r where
  gfunctor _ _ _ = identity

instance gfunctorCons
  :: ( Cons s (va -> g va) r0' r0
     , Cons s va r r2'
     , Cons s (g va) r r2
     , Functor g
     , GFunctor p f l0' r0' l1 r1 r2'
     , IsSymbol s
     , RGet Record SProxy s l0 r0
     , RModify p f SProxy s l2' r2' l2 r2
     , Semigroupoid p
     )
  => GFunctor p f (Cons s v l0') r0 l1 r1 r2
  where
  gfunctor l0 l1 record0 =
      rmodify l2' l2 s (rget l0 s record0)
        <<< (gfunctor l0' l1 record0')
    where
    l0' = TypeRow.RLProxy :: TypeRow.RLProxy l0'
    l2' = TypeRow.RLProxy :: TypeRow.RLProxy l2'
    l2 = TypeRow.RLProxy :: TypeRow.RLProxy l2
    s = SProxy :: SProxy s
    record0' :: Record r0'
    record0' = unsafeCoerce record0

mapRecord
  :: forall hl0 hr0   f l0 l1 p r0 r1 r2
   . Cut l0 l1 hl0
  => GFunctor p f hl0 hr0 l1 r1 r2
  => RowToList hl0 hr0
  => RowToList r0 l0
  => RowToList r1 l1
  => Record r0
  -> p (f r1) (f r2)
mapRecord =
  gfunctor
    (TypeRow.RLProxy :: TypeRow.RLProxy l0)
    (TypeRow.RLProxy :: TypeRow.RLProxy l1)




-- -- RECYCLE FROM module Record.Extra.MapRecord.GMapRecord
-- class GMapRecord
--   (p  :: Type -> Type -> Type)
--   (f :: # Type -> Type)
--   (l0 :: RowList)
--   (r0 :: # Type)
--   (l1 :: RowList)
--   (r1 :: # Type)
--   (r2 :: # Type)
--   | l0 -> r0
--   , l1 -> r1
--   , l0 l1 -> r2
--   where
--   gMapRecord
--     :: TypeRow.RLProxy l0
--     -> TypeRow.RLProxy l1
--     -> Record r0
--     -> p (f r1) (f r2)
--
-- -- RECYCLE FROM module Record.Extra.MapRecord.GMapRecord
-- instance gMapRecordNil :: Category p => GMapRecord p f Nil () l r r where
--   gMapRecord _ _ _ = identity
--
-- instance gMapRecord_FUNCTOR_Cons
--   :: ( Cons s (va -> vb) r0' r0
--      , Cons s va r r2'
--      , Cons s vb r r2
--      , RGet Record SProxy s l0 r0
--      , RModify p f SProxy s l2' r2' l2 r2
--
--      , GMapRecord p f l0' r0' l1 r1 r2'
--      , IsSymbol s
--      , Semigroupoid p
--
--      , GMapRecord_ True (Cons s v l0') l1 l2
--      , ListToRow l2 r2
--
--      , RGet f SProxy s l0 r0
--      , RInsert p f SProxy s l2' r2' l2 r2
--      , Semigroupoid p
--      , TypeEquals fn (v1 -> v2)
--      )
--   => GMapRecord p f (Cons s0 v0 l0') r0 (Cons s1 v1 l1') r1 (Cons s r2
--   where
--   gMapRecord l0 l1 record0 =
--       rmodify l2' l2 s (rget l0 s record0)
--         <<< (gMapRecord l0' l1 record0')
--     where
--     l0' = TypeRow.RLProxy :: TypeRow.RLProxy l0'
--     l2' = TypeRow.RLProxy :: TypeRow.RLProxy l2'
--     l2 = TypeRow.RLProxy :: TypeRow.RLProxy l2
--     s = SProxy :: SProxy s
--     record0' :: Record r0'
--     record0' = unsafeCoerce record0

-- instance gMapRecord_Cons
--   :: ( Cons s (va -> vb) r0' r0
--      , Cons s va r r2'
--      , Cons s vb r r2
--      , GMapRecord p f l0' r0' l1 r1 r2'
--      , IsSymbol s
--      , RGet Record SProxy s l0 r0
--      , RModify p f SProxy s l2' r2' l2 r2
--      , Semigroupoid p
--      )
--   => GMapRecord p f (Cons s v l0') r0 l1 r1 r2
--   where
--   gMapRecord l0 l1 record0 =
--       rmodify l2' l2 s (rget l0 s record0)
--         <<< (gMapRecord l0' l1 record0')
--     where
--     l0' = TypeRow.RLProxy :: TypeRow.RLProxy l0'
--     l2' = TypeRow.RLProxy :: TypeRow.RLProxy l2'
--     l2 = TypeRow.RLProxy :: TypeRow.RLProxy l2
--     s = SProxy :: SProxy s
--     record0' :: Record r0'
--     record0' = unsafeCoerce record0







-- RECYCLED
class HasSymbol (l :: RowList) (s :: Symbol) (b :: Boolean) | l s -> b
instance hasSymbol :: HasSymbol_ l s b True => HasSymbol l s b
class TypeEquals_
  (continue :: Boolean)
  (t0 :: Type)
  (t1 :: Type)
  | t0 -> t1
  , t1 -> t0
  where
  to :: t0 -> t1
  from :: t1 -> t0
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
message :: String
message = "'nonRefl' is a convenience instance of TypeEquals_. Its members should not be used."
instance nonRefl :: TypeEqualsTrue False t0 t1 where
  to = unsafeCrashWith message
  from x = unsafeCrashWith message
instance refl :: TypeEqualsTrue True t t where
  to   x = x
  from x = x
