module Record.Extra.GAlter where

import Prelude (identity)
import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row (class Cons, class Lacks, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)
import Data.RecordLike (class RGet, class RInsert, class RModify, rget, rinsert, rmodify)

class GAlter
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (r2 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l0 l1 -> r2
  where
  gAlter :: RLProxy l0 -> RLProxy l1 -> Record r0 -> Record r1 -> Record r2

instance gAlter_Nil :: GAlter Nil () l r r where
  gAlter _ _ _ = identity

instance gAlter_Cons_RModify
  :: ( Cons s (va -> vb) r0' r0
     , Cons s va r r2'
     , Cons s vb r r2
     , GAlter l0' r0' l1 r1 r2'
     , IsSymbol s
     , RGet Record SProxy s l0 r0
     , RModify Function Record SProxy s l2' r2' l2 r2
     )
  => GAlter (Cons s v l0') r0 l1 r1 r2
  where
  gAlter l0 l1 record0 src =
      rmodify l2' l2 s (rget l0 s record0) (gAlter l0' l1 record0' src)
    where
    l0' = (RLProxy :: RLProxy l0')
    l2' = (RLProxy :: RLProxy l2')
    l2 = (RLProxy :: RLProxy l2)
    s = (SProxy :: SProxy s)
    record0' :: Record r0'
    record0' = unsafeCoerce record0

else instance gAlter_Cons_RInsert
  :: ( Cons s v r0' r0
     , Cons s v r2' r2
     , GAlter l0' r0' l1 r1 r2'
     , IsSymbol s
     , Lacks s r2'
     , RGet Record SProxy s l0 r0
     , RInsert Function Record SProxy s l2' r2' l2 r2
     )
  => GAlter (Cons s v l0') r0 l1 r1 r2
  where
  gAlter l0 l1 record0 src =
      rinsert l2' l2 s (rget l0 s record0) (gAlter l0' l1 record0' src)
    where
    l0' = (RLProxy :: RLProxy l0')
    l2' = (RLProxy :: RLProxy l2')
    l2 = (RLProxy :: RLProxy l2)
    s = (SProxy :: SProxy s)
    record0' :: Record r0'
    record0' = unsafeCoerce record0

class RTransform
  (a  :: Type)
  (p  :: Type -> Type -> Type)
  (f  :: # Type -> Type)
  (g  :: Symbol -> Type)
  (s  :: Symbol)
  (v  :: Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rtransform
    :: a
    -> RLProxy l0
    -> RLProxy l1
    -> g s
    -> v
    -> p (f r0) (f r1)

data RInsert_ = RInsert_
data RModify_ = RModify_

instance rtransformRInsert
  :: ( RInsert p f g s l0 r0 l1 r1
     , Cons s v r0 r1
     , Lacks s r0
     , IsSymbol s
     )
  => RTransform RInsert_ p f g s v l0 r0 l1 r1
  where
  rtransform _ = rinsert

instance rtransformRModify
  :: ( RModify p f g s l0 r0 l1 r1
     , Cons s v0 r r0
     , Cons s v1 r r1
     , IsSymbol s
     )
  => RTransform RModify_ p f g s (v0 -> v1) l0 r0 l1 r1
  where
  rtransform _ = rmodify

