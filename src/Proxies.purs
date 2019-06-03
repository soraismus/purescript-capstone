module Type.Reify
  ( class BReify
  , class OReify
  , class RLReify
  , class RReify
  , class SReify
  , class TEReify
  , class TReify
  , class T2Reify
  , class T3Reify
  , bReify
  , oReify
  , rlReify
  , rReify
  , sReify
  , teReify
  , tReify
  , t2Reify
  , t3Reify
  ) where

import Data.Symbol (SProxy(SProxy))
import Prim.Boolean (kind Boolean)
import Prim.Ordering (kind Ordering)
import Type.Data.Boolean (BProxy(BProxy))
import Type.Data.Ordering (OProxy(OProxy))
import Type.Data.RowList (RLProxy(RLProxy)) as TypeDataRowList
import Type.Eval (TEProxy(TEProxy), kind TypeExpr)
import Type.Proxy (Proxy(Proxy), Proxy2(Proxy2), Proxy3(Proxy3))
import Type.Row (RProxy(RProxy), kind RowList)
import Type.Row (RLProxy(RLProxy)) as TypeRow

class BReify
  (f :: Boolean -> Type)
  (b :: Boolean)
  where
  bReify :: f b

instance bReifyBProxy :: BReify BProxy b where
  bReify = BProxy

class OReify
  (f :: Ordering -> Type)
  (o :: Ordering)
  where
  oReify :: f o

instance oReifyOProxy :: OReify OProxy o where
  oReify = OProxy

class RLReify
  (f :: RowList -> Type)
  (l :: RowList)
  where
  rlReify :: f l

instance rlReify_TypeRow_RLProxy
  :: RLReify TypeRow.RLProxy l
  where
  rlReify = TypeRow.RLProxy

else instance rlReify_TypeDataRowList_RLProxy
  :: RLReify TypeDataRowList.RLProxy l
  where
  rlReify = TypeDataRowList.RLProxy

class RReify
  (f :: # Type -> Type)
  (r :: # Type)
  where
  rReify :: f r

instance rReifyRProxy :: RReify RProxy r where
  rReify = RProxy

class SReify
  (f :: Symbol -> Type)
  (s :: Symbol)
  where
  sReify :: f s

instance sReifySProxy :: SReify SProxy s where
  sReify = SProxy

class TEReify
  (f :: TypeExpr -> Type)
  (e :: TypeExpr)
  where
  teReify :: f e

instance teReifyTEProxy :: TEReify TEProxy e where
  teReify = TEProxy

class TReify (f :: Type -> Type) (a :: Type) where tReify :: f a

instance tReifyProxy :: TReify Proxy a where
  tReify = Proxy

class T2Reify
  (f :: (Type -> Type) -> Type)
  (a :: Type -> Type)
  where
  t2Reify :: f a

instance t2ReifyT1Proxy :: T2Reify Proxy2 a where
  t2Reify = Proxy2

class T3Reify
  (f :: (Type -> Type -> Type) -> Type)
  (a :: Type -> Type -> Type)
  where
  t3Reify :: f a

instance t3ReifyProxy3 :: T3Reify Proxy3 a where
  t3Reify = Proxy3
