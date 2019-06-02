module Record.Extra.Utils where

import Prelude (identity)
import Data.Symbol (class IsSymbol, SProxy(SProxy), reflectSymbol)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row
  ( class Cons
  , class Lacks
  , Cons
  , Nil
  , kind RowList
  )
import Unsafe.Coerce (unsafeCoerce)
import Data.RecordLike (class RGet, class RInsert, rget, rinsert)







import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Type.Row (class ListToRow, Cons, Nil)

singleton
  :: forall r s v
   . IsSymbol s
  => ListToRow (Cons s v Nil) r
  => SProxy s
  -> v
  -> Record r
singleton sProxy value =
  unsafeSingleton (reflectSymbol sProxy) value

foreign import unsafeSingleton :: forall r a. String -> a -> Record r


-- -- -- -- -- -- -

class EmbedIntoFunction
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (r2 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l0 l1 -> r2
  where
  embed :: RLProxy l0 -> RLProxy l1 -> Record r0 -> Record r1 -> Record r2

instance embedIntoFunction_Nil :: EmbedIntoFunction Nil () l r r where
  embed _ _ _ = identity

instance embedIntoFunction_Cons
  :: ( Cons s v r0' r0
     , Cons s v r2' r2
     , EmbedIntoFunction l0' r0' l1 r1 r2'
     , IsSymbol s
     , Lacks s r0'
     , Lacks s r2'
     , RGet Record SProxy s l0 r0
     , RInsert Record SProxy s l2' r2' l2 r2
     )
  => EmbedIntoFunction (Cons s v l0') r0 l1 r1 r2
  where
  embed l0 l1 record0 src =
      rinsert l2' l2 s (rget l0 s record0) (embed l0' l1 record0' src)
    where
    l0' = (RLProxy :: RLProxy l0')
    l2' = (RLProxy :: RLProxy l2')
    l2 = (RLProxy :: RLProxy l2)
    s = (SProxy :: SProxy s)
    record0' :: Record r0'
    record0' = unsafeCoerce record0