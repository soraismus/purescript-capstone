module Record.Extra.MapRecord.GMapRecord
  ( class GMapRecord
  , gMapRecord
  )
  where

import Prelude (class Category, class Semigroupoid, identity, (<<<))

import Data.RecordLike (class RGet, class RModify, rget, rmodify)
import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Type.Row (class Cons, class RowToList, Cons, Nil, kind RowList)
import Type.Row (RLProxy(RLProxy)) as TypeRow
import Unsafe.Coerce (unsafeCoerce)

class GMapRecord
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
  gMapRecord
    :: TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> Record r0
    -> p (f r1) (f r2)

instance gMapRecord_Nil :: Category p => GMapRecord p f Nil () l r r where
  gMapRecord _ _ _ = identity

instance gMapRecord_Cons
  :: ( Cons s (va -> vb) r0' r0
     , Cons s va r r2'
     , Cons s vb r r2
     , GMapRecord p f l0' r0' l1 r1 r2'
     , IsSymbol s
     , RGet Record SProxy s l0 r0
     , RModify p f SProxy s l2' r2' l2 r2
     , Semigroupoid p
     )
  => GMapRecord p f (Cons s v l0') r0 l1 r1 r2
  where
  gMapRecord l0 l1 record0 =
      rmodify l2' l2 s (rget l0 s record0)
        <<< (gMapRecord l0' l1 record0')
    where
    l0' = TypeRow.RLProxy :: TypeRow.RLProxy l0'
    l2' = TypeRow.RLProxy :: TypeRow.RLProxy l2'
    l2 = TypeRow.RLProxy :: TypeRow.RLProxy l2
    s = SProxy :: SProxy s
    record0' :: Record r0'
    record0' = unsafeCoerce record0
