module Data.Struct.RSingleton
  ( class RSingleton
  , rsingleton
  ) where

import Data.Symbol (class IsSymbol, SProxy)
import Record.Extra.Utils (singleton) as RecordExtra
import Type.Row (class ListToRow, Cons , Nil)

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
