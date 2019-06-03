module Record.Extra.GMapRecord where

import Prelude (identity)
import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row (class Cons, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)
import Data.RecordLike (class RGet, class RModify, rget, rmodify)

class GMapRecord
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (r2 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l0 l1 -> r2
  where
  gMapRecord :: RLProxy l0 -> RLProxy l1 -> Record r0 -> Record r1 -> Record r2

instance gMapRecord_Nil :: GMapRecord Nil () l r r where
  gMapRecord _ _ _ = identity

instance gMapRecord_Cons
  :: ( Cons s (va -> vb) r0' r0
     , Cons s va r r2'
     , Cons s vb r r2
     , GMapRecord l0' r0' l1 r1 r2'
     , IsSymbol s
     , RGet Record SProxy s l0 r0
     , RModify Function Record SProxy s l2' r2' l2 r2
     )
  => GMapRecord (Cons s v l0') r0 l1 r1 r2
  where
  gMapRecord l0 l1 record0 src =
      rmodify l2' l2 s (rget l0 s record0) (gMapRecord l0' l1 record0' src)
    where
    l0' = (RLProxy :: RLProxy l0')
    l2' = (RLProxy :: RLProxy l2')
    l2 = (RLProxy :: RLProxy l2)
    s = (SProxy :: SProxy s)
    record0' :: Record r0'
    record0' = unsafeCoerce record0
