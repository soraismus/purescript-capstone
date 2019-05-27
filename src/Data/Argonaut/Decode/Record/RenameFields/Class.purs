module Data.Argonaut.Decode.Record.RenameFields.Class
  ( class RenameFields
  , renameFields
  ) where

import Data.SameKeys (class SameKeys)
import Data.SameSize (class SameSize)
import Data.SameValues (class SameValues)
import Data.Symbol (class IsSymbol, SProxy(SProxy))
import Record (get, insert)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
import Type.Row
  ( class Cons
  , class Lacks
  , class RowToList
  , Cons
  , Nil
  , kind RowList
  )
import Unsafe.Coerce (unsafeCoerce)

class RenameFields
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l2 :: RowList)
  (r2 :: # Type)
  | l1 -> l0 r0 r1 l2 r2 where
  renameFields
    :: RLProxy l0
    -> RLProxy l1
    -> RLProxy l2
    -> Record r1
    -> Record r0
    -> Record r2

instance renameFieldsNil :: RenameFields Nil () Nil () Nil () where
  renameFields _ _ _ _ _ = {}

instance renameFieldsCons
  :: ( Cons origS v r0' r0
     --, Cons origS dv dr' dr
     , Cons origS (SProxy newS) dr' dr
     , Cons newS v r2' r2
     , IsSymbol origS
     , IsSymbol newS
     , Lacks origS r0'
     , Lacks origS dr'
     , Lacks newS r2'
     , RenameFields l0' r0' dl' dr' l2' r2'
     , RowToList r0 l0
     , RowToList r0' l0'
     , RowToList dr dl
     , RowToList dr' dl'
     , RowToList r2 l2
     , RowToList r2' l2'
     , SameKeys l0 dr
     , SameKeys l0' dr'
     , SameSize dl r2
     , SameSize dl' r2'
     , SameValues l0 r2
     , SameValues l0' r2'
     --, TypeEquals dv (SProxy newS)
     )
  => RenameFields
        (Cons origS v l0')
        r0
        --(Cons origS dv dl')
        (Cons origS (SProxy newS) dl')
        dr
        (Cons newS v l2')
        r2
  where
  renameFields _ _ _ decoderRecord origRecord =
    let
      origSProxy :: SProxy origS
      origSProxy = SProxy

      newSProxy :: SProxy newS
      newSProxy = SProxy

      -- To prevent unnecessary creation of intermediate records,
      -- coercion is used rather than calling `Record.delete sProxy`
      -- to induce the next expected type.
      decoderRecord' :: Record dr'
      decoderRecord' = unsafeCoerce decoderRecord

      origRecord' :: Record r0'
      origRecord' = unsafeCoerce origRecord

      newRecord' :: Record r2'
      newRecord' =
        renameFields
          (RLProxy :: RLProxy l0')
          (RLProxy :: RLProxy dl')
          (RLProxy :: RLProxy l2')
          decoderRecord'
          origRecord'

    in
      insert newSProxy (get origSProxy origRecord) newRecord'
