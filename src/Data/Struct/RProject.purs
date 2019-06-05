module Data.Struct.RProject
  ( class RProject
  , rproject
  ) where

import Prelude (pure, ($))

import Control.Alternative (class Alternative)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant)
import Data.Variant (prj) as Variant
import Record (get) as Record
import Type.Row (class Cons, kind RowList)
import Type.Row (RLProxy) as TypeRow

class RProject
  (f  :: # Type -> Type)
  (g  :: Symbol -> Type)
  (s  :: Symbol)
  (l :: RowList)
  (r :: # Type)
  | l -> r
  where
  rproject
    :: forall h r' v
     . Alternative h
    => Cons s v r' r
    => TypeRow.RLProxy l
    -> g s
    -> f r
    -> h v

instance rprojectRecord :: IsSymbol s => RProject Record SProxy s l r where
  rproject _ s record = pure $ Record.get s record

instance rprojectVariant :: IsSymbol s => RProject Variant SProxy s l r where
  rproject _ = Variant.prj
