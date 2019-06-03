module Data.RecordLike.RSingleton
  ( class RSingleton
  , rsingleton
  ) where

import Prelude (Ordering, const, eq, identity, pure, ($), (<<<))
import Control.Alternative (class Alternative)
import Data.Symbol (class IsSymbol, SProxy)
import Record.Extra.Utils (singleton) as RecordExtra
import Type.Row
  ( class Cons
  , class Lacks
  , class ListToRow
  , class Nub
  , class RowToList
  , class Union
  , Cons
  , Nil
  , RProxy(RProxy)
  , kind RowList
  )
import Type.Row (RLProxy) as TypeRow
import Unsafe.Coerce (unsafeCoerce)

class RSingleton
  (f :: # Type -> Type)
  (g :: Symbol -> Type)
  (s :: Symbol)
  where
  rsingleton :: forall r v. ListToRow (Cons s v Nil) r => g s -> v -> f r

instance rsingletonRecord
  :: IsSymbol s
  => RSingleton Record SProxy s
  where
  rsingleton = RecordExtra.singleton
