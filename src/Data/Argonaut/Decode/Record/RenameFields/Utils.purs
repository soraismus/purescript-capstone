module Data.Argonaut.Decode.Record.RenameFields.Utils
  ( renameFields
  ) where

import Data.SameKeys (class SameKeys)
import Data.SameSize (class SameSize)
import Data.SubFields (class SubFields)

import Data.Argonaut.Decode.Record.Instantiate.Class (class Instantiate)
import Data.Argonaut.Decode.Record.RenameFields.Class
  ( class RenameFields
  , renameFields
  ) as D
import Data.Argonaut.Decode.Record.Utils (getSubRecord)
import Record (union)
import Type.Data.RowList (RLProxy(RLProxy)) -- Argonaut dependency
--import Type.Row (class Nub, class RowToList, class Union)
import Type.Row (class RowToList, class Union)

-- disjointUnion, merge, union
-- merge :: Union, Nub r3 r4 => -- records can overlap; removes duplicate labels
-- union :: Union => -- doesn't remove duplicate labels; records can overlap
-- disjointUnion :: Union, Nub r3 r3 => -- records can't overlap

renameFields
  :: forall l0 l1 l2 l3 l4 l5 r0 r1 r1' r2 r3 r3' r4 r5
   -- . D.RenameFields l0 r0 l1 r1 l2 r2
   . D.RenameFields l1 r1 l0 r0 l2 r2
  -- => Nub r2 r2
  => Instantiate l1 r1'
  => Instantiate l3 r3'
  => RowToList r0 l0
  => RowToList r1 l1
  => RowToList r2 l2
  => RowToList r3 l3
  => RowToList r4 l4
  => RowToList r5 l5
  -- => SameKeys l1 r2
  => SameKeys l1 r1
  => SameKeys l3 r3
  -- => SameSize l1 r2
  => SameSize l1 l1
  => SameSize l3 l3
  -- => SubFields l2 r4
  => SubFields l1 r4
  => SubFields l3 r4
  => Union r1 r3 r4
  => Union r2 r3 r5
  => Record r0
  -> Record r4
  -> Record r5
renameFields renamingRecord record =
    union subRecord0 subRecord1
  where
  subRecord0 :: Record r2
  subRecord0 =
    D.renameFields
        (RLProxy :: RLProxy l1)
        (RLProxy :: RLProxy l0)
        (RLProxy :: RLProxy l2)
        renamingRecord
        (getSubRecord (RLProxy :: RLProxy l1) record)
  subRecord1 :: Record r3
  subRecord1 = getSubRecord (RLProxy :: RLProxy l3) record

-- decodeJsonWith'
--   :: forall dr dl f l0 l1 l2 r0 r1 r2
--    . Bind f
--   => D.DecodeJsonWith f dl dr l0 r0
--   => GDecodeJson r1 l1
--   => RowToList r1 l1
--   => RowToList r2 l2
--   => RowToList dr dl
--   => Status f
--   => Union r0 r1 r2
--   => Record dr
--   -> Json
--   -> f (Record r2)
-- decodeJsonWith' decoderRecord = reportJson go
--   where
--   go :: Object Json -> f (Record r2)
--   go object = do
--     record0 <-
--       D.decodeJsonWith
--         (RLProxy :: RLProxy l0)
--         (RLProxy :: RLProxy dl)
--         decoderRecord
--         object
--     record1 <- reportObject object (RLProxy :: RLProxy l1)
--     report $ union record0 record1
