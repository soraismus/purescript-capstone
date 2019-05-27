module Data.Argonaut.Decode.Record.Instantiate.Class
  ( class Instantiate
  , instantiate
  ) where

import Prelude

-- import Prelude (class Bind, bind, ($))
-- import Data.Argonaut.Core (Json)
-- import Data.Argonaut.Decode.Record.Utils (getMissingFieldErrorMessage)
-- import Data.Maybe (Maybe(Just, Nothing))
import Data.SameKeys (class SameKeys)
-- import Data.Status (class Status, report, reportError)
import Data.Symbol (class IsSymbol, SProxy(SProxy))
-- import Foreign.Object (Object, lookup)
import Record (insert)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row
  ( class Cons
  , class Lacks
  , class RowToList
  , Cons
  , Nil
  , kind RowList
  )

class Instantiate (l :: RowList) (r :: # Type) | l -> r where
  instantiate :: RLProxy l -> Record r

instance instantiateNil :: Instantiate Nil () where
  instantiate _ = {}

instance instantiateCons
  :: ( Cons s v r0' r0
     , Cons s Unit r1' r1
     , Instantiate l0' r1'
     , IsSymbol s
     , Lacks s r0'
     , Lacks s r1'
     , RowToList r0 l0
     , RowToList r0' l0'
     , RowToList r1 (Cons s Unit l1')
     , RowToList r1' l1'
     , SameKeys l0 r1
     , SameKeys l0' r1'
     )
  => Instantiate (Cons s v l0') r1
  where
  instantiate _ =
    insert (SProxy :: SProxy s) unit
      $ instantiate (RLProxy :: RLProxy l0')
