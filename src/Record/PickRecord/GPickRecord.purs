module Record.Extra.PickRecord.GPickRecord
  ( class GPickRecord
  , gPickRecord
  ) where

import Prelude (class Category, class Semigroupoid, identity, (<<<))

import Data.RecordLike
  ( class RConst
  , class RDelete
  , class REmpty
  , rconst
  , rdelete
  , rempty
  )
import Data.Symbol (SProxy(SProxy))
import Type.Row (class Cons, class Lacks, Cons, Nil, kind RowList)
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
  => GPickRecord p f Nil () (Cons s1 v1 l1') unifyR (Cons s2 v2 l2') unifyR
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
