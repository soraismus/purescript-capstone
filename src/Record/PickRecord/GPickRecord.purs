module Record.Extra.PickRecord.GPickRecord
  ( class GPickRecord
  , gPickRecord
  ) where

import Prelude (class Category, class Semigroupoid, identity, (<<<))

import Data.RecordLike (class RDelete, rdelete)
import Data.Symbol (class IsSymbol, SProxy(SProxy))
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
  , l0 l1 -> l2 r2
  where
  gPickRecord
    :: TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> TypeRow.RLProxy l2
    -> p (f r1) (f r2)

instance gPickRecord_Nil
  :: Category p
  => GPickRecord p f Nil () l r l r
  where
  gPickRecord _ _ _ = identity

instance gPickRecord_ConsConsCons
  :: ( Cons s v r2' r2
     , GPickRecord p f l0' r0' l1 r1 l2 r2
     , IsSymbol s
     , Lacks s r2'
     , RDelete p f SProxy s l2 r2 l2' r2'
     , Semigroupoid p
     )
  => GPickRecord p f (Cons s v l0') r0 l1 r1 l2' r2'
  where
  gPickRecord l0 l1 l2' =
    rdelete l2 l2' s <<< gPickRecord l0' l1 l2
    where
    l0' = TypeRow.RLProxy :: TypeRow.RLProxy l0'
    l2 = TypeRow.RLProxy :: TypeRow.RLProxy l2
    s = SProxy :: SProxy s
