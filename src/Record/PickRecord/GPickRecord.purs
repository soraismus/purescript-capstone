module Record.Extra.PickRecord.GPickRecord
  ( class GPickRecord
  , gPickRecord
  ) where

import Prelude (class Category, class Semigroupoid, identity, (<<<))

import Data.RecordLike (class RConst, class RDelete, class REmpty, rconst, rdelete, rempty)
import Data.Symbol (SProxy(SProxy))
-- import Record.Extra.SubFields (class SubFields)
import Type.Row
  ( class Cons
  , class Lacks
  , class ListToRow
  , class RowToList
  , class Union
  , Cons
  , Nil
  , kind RowList
  )
import Type.Row (RLProxy(RLProxy)) as TypeRow

class GPickRecord
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l2 :: RowList)
  (r2 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l2 -> r2
  , l0 l1 -> l2
  where
  gPickRecord
    :: TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> TypeRow.RLProxy l2
    -> p (f r1) (f r2)

instance gPickRecord_Nil_Nil_Nil
  :: Category p
  => GPickRecord p f Nil () Nil () Nil ()
  where
  gPickRecord _ _ _ = identity

instance gPickRecord_Nil_Cons_Cons
  :: Category p
  => GPickRecord p f Nil () (Cons s v l') r (Cons s v l') r
  where
  gPickRecord _ _ _ = identity

instance gPickRecord_Cons_Cons_Nil
  :: ( RConst p f Nil () (Cons s v l') r
     , REmpty f
     )
  => GPickRecord p f (Cons s v l') r (Cons s v l') r Nil ()
  where
  gPickRecord _ l nil = rconst nil l rempty

else instance gPickRecord_Cons_Cons_Cons
  :: ( Cons s0 v0 r2' r2
     , GPickRecord
          p
          f
          l0'
          r0'
          (Cons s1 v1 l1')
          r1
          (Cons s0 v0 (Cons s2' v2' l2''))
          r2
     , Lacks s0 r2'
     , RDelete
          p
          f
          SProxy
          s0
          (Cons s0 v0 (Cons s2' v2' l2''))
          r2
          (Cons s2' v2' l2'')
          r2'
     , Semigroupoid p
     )
  => GPickRecord
        p
        f
        (Cons s0 v0 l0')
        r0
        (Cons s1 v1 l1')
        r1
        (Cons s2' v2' l2'')
        r2'
  where
  gPickRecord l0 l1 l2' =
      rdelete l2 l2' s0 <<< gPickRecord l0' l1 l2
    where
    l0' = TypeRow.RLProxy :: TypeRow.RLProxy l0'
    l2 = TypeRow.RLProxy :: TypeRow.RLProxy (Cons s0 v0 (Cons s2' v2' l2''))
    s0 = SProxy :: SProxy s0


-- instance gPickRecord_ConsOne_ConsOne
--   :: Category p
--   => GPickRecord p f (Cons s v Nil) r (Cons s v Nil) r
--   where
--   gPickRecord l0 l1 = identity

-- instance gPickRecord_ConsMany_ConsOne
--   :: ( Cons s1 v1 r1' r1
--      , GPickRecord
--           p
--           f
--           (Cons s1' v1' l1'')
--           r1'
--           (Cons s2 v2 Nil)
--           r2
--      , Lacks s1 r1'
--      , RDelete
--           p
--           f
--           SProxy
--           s1
--           (Cons s1 v1 (Cons s1' v1' l1''))
--           r1
--           (Cons s1' v1' l1'')
--           r1'
--      , Semigroupoid p
--      )
--   => GPickRecord p f (Cons s1 v1 (Cons s1' v1' l1'')) r1 (Cons s2 v2 Nil) r2
--   where
--   gPickRecord l1 l2 =
--     gPickRecord l1' l2 <<< rdelete l1 l1' s1
-- --       rdelete l1 l1' s1 <<< gPickRecord l1' nil
--     where
--     l1' = TypeRow.RLProxy :: TypeRow.RLProxy (Cons s1' v1' l1'')
--     s1 = SProxy :: SProxy s1

-- instance gPickRecord_ConsMany_ConsOne
--   :: ( Cons s v r2' r2
--      , GPickRecord p f (Cons s1 v1 (Cons s1' v1' l1'')) r1 (Cons s v (Cons s2 v2 Nil)) r2
--      , Lacks s r2'
--      , RDelete p f SProxy s (Cons s v (Cons s2 v2 Nil)) r2 (Cons s2 v2 Nil) r2'
--      , RowToList r (Cons s v l')
--      , Semigroupoid p
--      , Union r2 r r1
--      )
--   => GPickRecord p f (Cons s1 v1 (Cons s1' v1' l1'')) r1 (Cons s2 v2 Nil) r2'
--   where
--   gPickRecord l1 l2' =
--       rdelete l2 l2' s <<< gPickRecord l1 l2
--     where
--     l2 = TypeRow.RLProxy :: TypeRow.RLProxy (Cons s v (Cons s2 v2 Nil))
--     s = SProxy :: SProxy s

-- instance gPickRecord_ConsMany_ConsMany
--   :: ( Cons s v r2' r2
--      , GPickRecord
--           p
--           f
--           (Cons s1 v1 (Cons s1' v1' l1''))
--           r1
--           (Cons s v (Cons s2 v2 Nil))
--           r2
--      , Lacks s r2'
--      , RDelete
--           p
--           f
--           SProxy
--           s
--           (Cons s v (Cons s2 v2 Nil))
--           r2
--           (Cons s2 v2 Nil)
--           r2'
--      , RowToList r (Cons s v l')
--      , Semigroupoid p
--      , Union r2 r r1
--      )
--   => GPickRecord
--         p
--         f
--         (Cons s1 v1 (Cons s1' v1' l1''))
--         r1
--         (Cons s2 v2 (Cons s2' v2' l2''))
--         r2'
--   where
--   gPickRecord l1 l2' =
--       rdelete l2 l2' s <<< gPickRecord l1 l2
--     where
--     l2 = TypeRow.RLProxy :: TypeRow.RLProxy (Cons s v (Cons s2 v2 (Cons s2' v2' l2'')))
--     s = SProxy :: SProxy s















-- instance gPickRecord_ConsMany_ConsMany
--   :: ( Cons s1 v1 r1' r1
--      , GPickRecord
--           p
--           f
--           (Cons s1' v1' l1'')
--           r1'
--           (Cons s2 v2 (Cons s2' v2' l2''))
--           r2
--      , Lacks s1 r1'
--      , RDelete
--           p
--           f
--           SProxy
--           s1
--           (Cons s1 v1 (Cons s1' v1' l1''))
--           r1
--           (Cons s1' v1' l1'')
--           r1'
--      , Semigroupoid p
--      )
--   => GPickRecord
--         p
--         f
--         (Cons s1 v1 (Cons s1' v1' l1''))
--         r1
--         (Cons s2 v2 (Cons s2' v2' l2''))
--         r2
--   where
--   gPickRecord l1 l2 =
--     gPickRecord l1' l2 <<< rdelete l1 l1' s1
-- --       rdelete l1 l1' s1 <<< gPickRecord l1' nil
--     where
--     l1' = TypeRow.RLProxy :: TypeRow.RLProxy (Cons s1' v1' l1'')
--     s1 = SProxy :: SProxy s1





-- instance gPickRecord_NilNilNil
--   :: Category p
--   => GPickRecord p f Nil Nil () Nil ()
--   where
--   gPickRecord _ _ = identity
--
-- instance gPickRecord_ConsConsNil
--   :: ( Cons s0 v0 () r2
--      , GPickRecord p f l0' (Cons s1 v1 l1') r1 (Cons s0 v0 Nil) r2
--      , Lacks s0 ()
--      , ListToRow (Cons s0 v0 l0') r0
--      , RDelete p f SProxy s0 (Cons s0 v0 Nil) r2 Nil ()
--      , Semigroupoid p
--      --, Union r0 () r1
--      , Union () r0 r1
--      )
--   => GPickRecord p f (Cons s0 v0 l0') (Cons s1 v1 l1') r1 Nil ()
--   where
--   gPickRecord l1 nil =
--     rdelete l2 nil s0 <<< gPickRecord l1 l2
--     where
--     l2 = TypeRow.RLProxy :: TypeRow.RLProxy (Cons s0 v0 Nil)
--     s0 = SProxy :: SProxy s0
--
-- instance gPickRecord_NilConsCons
--   :: ( Category p
--      , SubFields (Cons s1 v1 l1') r
--      )
--   -- => GPickRecord p f Nil (Cons s1 v1 l1') r (Cons s2 v2 l2') r
--   => GPickRecord p f Nil (Cons s1 v1 l1') r (Cons s2 v2 l2') r
--   where
--   gPickRecord _ _ = identity
--
-- -- WORK WITH L1 and L2, NOT L0.
-- else instance gPickRecord_ConsConsCons
--   :: ( Cons s0 v0 r2' r2
--      , GPickRecord
--           p
--           f
--           l0'
--           (Cons s1 v1 l1')
--           r1
--           (Cons s0 v0 (Cons s2 v2 l2''))
--           r2
--      , Lacks s0 r2'
--      , ListToRow (Cons s0 v0 l0') r0
--      , RDelete
--           p
--           f
--           SProxy
--           s0
--           (Cons s0 v0 (Cons s2 v2 l2''))
--           r2
--           (Cons s2 v2 l2'')
--           r2'
--      , Semigroupoid p
--      --, Union r0 r2' r1
--      , Union r2' r0 r1
--      )
--   => GPickRecord p f (Cons s0 v0 l0') (Cons s1 v1 l1') r1 (Cons s2 v2 l2'') r2'
--   where
--   gPickRecord l1 l2' =
--     rdelete l2 l2' s0 <<< gPickRecord l1 l2
--     where
--     l2 = TypeRow.RLProxy :: TypeRow.RLProxy (Cons s0 v0 (Cons s2 v2 l2''))
--     s0 = SProxy :: SProxy s0


-- -- Is this just `Union` but for RowLists?
-- class RowListDiff_
--   (l0 :: RowList)
--   (l1 :: RowList)
--   (l2 :: RowList)
--   (continue :: Boolean)
--   | l0 l1 -> l2
--   , l0 l2 -> l1
--   , l1 l2 -> l0
-- instance rowListDiff_False :: RowListDiff_ l0 l1 l2 False
-- instance rowListDiff_Nil_Nil_Nil_ :: RowListDiff_ Nil Nil Nil True
-- instance rowListDiff_Cons_Nil_Cons_ :: RowListDiff_ (Cons s v l) Nil (Cons s v l) True
-- instance rowListDiff_Cons_Cons_Nil_
--   :: ( ListToRow (Cons s0 v0 l0) r0
--      -- SubFields and SameSize ==> EQUALS
--      , SubFields (Cons s1 v1 l1) r0
--      , SameSize (Cons s0 v0 l0) (Cons s1 v1 l1)
--      )
--   => RowListDiff_ (Cons s0 v0 l0) (Cons s1 v1 l1) Nil True
-- instance rowListDiff_Cons_Cons_Cons_
--   :: (
--      , Equals s0 s1 eq
--      , Not eq uneq
--      , If eq
--           ()
--           ()
--           ()
--      , HasSymbol_ l1' s0 has uneq
--      , Not has lacks
--      , And uneq has uneqAndHas
--      , And uneq lacks uneqAndLacks
--      , RowListDiff_ l0' l1' (Cons s2 v2 l2') eq
--      )
--   => RowListDiff_ (Cons s0 v0 l0') (Cons s1 v1 l1') (Cons s2 v2 l2') True
