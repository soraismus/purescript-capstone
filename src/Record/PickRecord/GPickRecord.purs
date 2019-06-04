module Record.Extra.PickRecord.GPickRecord
  ( class GPickRecord
  , gPickRecord
  ) where

import Prelude (class Category, class Semigroupoid, identity, (<<<))

import Data.RecordLike (class RDelete, rdelete)
import Data.Symbol (SProxy(SProxy))
import Type.Row (class Cons, class Lacks, Cons, Nil, kind RowList)
import Type.Row (RLProxy(RLProxy)) as TypeRow

     -- Are both `continuationL` and `continuationR` necessary?
class GPickRecord
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l2 :: RowList)
  (r2 :: # Type)
  | l1 -> r1
  , l2 -> r2
  , l1 l2 -> l0 r0
  where
  gPickRecord
    :: TypeRow.RLProxy l1
    -> TypeRow.RLProxy l2
    -> p (f r1) (f r2)

instance gPickRecord_NilNilNil
  :: Category p
  => GPickRecord p f Nil () Nil () Nil ()
  where
  gPickRecord _ _ = identity

instance gPickRecord_ConsConsNil
  :: ( Cons s0 v0 () r2
     , GPickRecord p f l0' r0' (Cons s1 v1 l1') r1 (Cons s0 v0 Nil) r2
     , Lacks s0 ()
     , RDelete p f SProxy s0 (Cons s0 v0 Nil) r2 Nil ()
     , Semigroupoid p
     )
  => GPickRecord p f (Cons s0 v0 l0') r0 (Cons s1 v1 l1') r1 Nil ()
  where
  gPickRecord l1 nil =
    rdelete l2 nil s0 <<< gPickRecord l1 l2
    where
    l2 = TypeRow.RLProxy :: TypeRow.RLProxy (Cons s0 v0 Nil)
    s0 = SProxy :: SProxy s0

instance gPickRecord_NilConsCons
  :: Category p
  => GPickRecord p f Nil () (Cons s v l') r (Cons s v l') r
  where
  gPickRecord _ _ = identity

else instance gPickRecord_ConsConsCons
  :: ( Cons s0 v0 r2' r2
     , GPickRecord p f l0' r0' (Cons s1 v1 l1') r1 (Cons s0 v0 (Cons s2 v2 l2'')) r2
     , Lacks s0 r2'
     , RDelete p f SProxy s0 (Cons s0 v0 (Cons s2 v2 l2'')) r2 (Cons s2 v2 l2'') r2'
     , Semigroupoid p
     )
  => GPickRecord p f (Cons s0 v0 l0') r0 (Cons s1 v1 l1') r1 (Cons s2 v2 l2'') r2'
  where
  gPickRecord l1 l2' =
    rdelete l2 l2' s0 <<< gPickRecord l1 l2
    where
    l2 = TypeRow.RLProxy :: TypeRow.RLProxy (Cons s0 v0 (Cons s2 v2 l2''))
    s0 = SProxy :: SProxy s0

-- instance gPickRecord_ConsConsNil
--   :: ( Cons s0 v0 () rx
--      , GPickRecord p f l0' r0' (Cons s v l1') r Nil ()
--      , Lacks s0 ()
--      , RDelete p f SProxy s0 kl2 kr2 Nil ()
--      -- Are both `continuationL` and `continuationR` necessary?
--      , GPickRecord p f kl0 kr0 (Cons s1 v1 l1') r1 kl2 kr2
--      , HasSymbol_ l? s? alternativeOutput? uneq
--      , Equals s0 s1 eq
--      , Not eq uneq
--      , If eq
--          (RLandRL l0'        (Cons s0 v0 Nil))
--          (RLandRL kl0_ifUneq kl2_ifUneq      )
--          (RLandRL kl0        kl2             )
--      )
--   => GPickRecord p f (Cons s0 v0 l0') r0 (Cons s1 v1 l1') r1 Nil ()
--   where
--   gPickRecord l nil =
--     rdelete lx nil s <<< gPickRecord l1 lx

-- instance gPickRecord_Nil
--   :: Category p
--   => GPickRecord p f () l r l r
--   where
--   gPickRecord _ _ = identity
--
-- instance gPickRecord_ConsConsCons
--   :: ( Cons s v r2' r2
--      , GPickRecord p f r0' l1 r1 l2 r2
--      , IsSymbol s
--      , Lacks s r2'
--      , RDelete p f SProxy s l2 r2 l2' r2'
--      , Semigroupoid p
--      )
--   => GPickRecord p f r0 l1 r1 l2' r2'
--   where
--   gPickRecord l1 l2' =
--     rdelete l2 l2' s <<< gPickRecord l1 l2
--     where
--     l0' = TypeRow.RLProxy :: TypeRow.RLProxy l0'
--     l2 = TypeRow.RLProxy :: TypeRow.RLProxy l2
--     s = SProxy :: SProxy s
