module Data.RecordLike
  ( module Data.RecordLike.RCompare
  , module Data.RecordLike.RConst
  , module Data.RecordLike.RContract
  , module Data.RecordLike.RDelete
  , module Data.RecordLike.RDisjointUnion
  , module Data.RecordLike.REmpty
  , module Data.RecordLike.REqual
  , module Data.RecordLike.RExpand
  , module Data.RecordLike.REval
  , module Data.RecordLike.RGet
  , module Data.RecordLike.RInsert
  , module Data.RecordLike.RMatch
  , module Data.RecordLike.RMerge
  , module Data.RecordLike.RModify
  , module Data.RecordLike.RNub
  , module Data.RecordLike.ROn
  , module Data.RecordLike.ROnMatch
--   , module Data.RecordLike.RPick
  , module Data.RecordLike.RProject
  , module Data.RecordLike.RRename
  , module Data.RecordLike.RSet
  , module Data.RecordLike.RSingleton
  , module Data.RecordLike.RUnion
  ) where

import Data.RecordLike.RCompare (class RCompare, rcompare)
import Data.RecordLike.RConst (class RConst, rconst)
import Data.RecordLike.RContract (class RContract, rcontract)
import Data.RecordLike.RDelete (class RDelete, rdelete)
import Data.RecordLike.RDisjointUnion (class RDisjointUnion, rdisjointUnion)
import Data.RecordLike.REmpty (class REmpty, rempty)
import Data.RecordLike.REqual (class REqual, requal)
import Data.RecordLike.REval (class REval, reval)
import Data.RecordLike.RExpand (class RExpand, rexpand)
import Data.RecordLike.RGet (class RGet, rget)
import Data.RecordLike.RInsert (class RInsert, rinsert)
import Data.RecordLike.RMatch (class RMatch, rmatch)
import Data.RecordLike.RMerge (class RMerge, rmerge)
import Data.RecordLike.RModify (class RModify, rmodify)
import Data.RecordLike.RNub (class RNub, rnub)
import Data.RecordLike.ROn (class ROn, ron)
import Data.RecordLike.ROnMatch (class ROnMatch, ronMatch)
-- import Data.RecordLike.RPick (class RPick, rpick)
import Data.RecordLike.RProject (class RProject, rproject)
import Data.RecordLike.RRename (class RRename, rrename)
import Data.RecordLike.RSet (class RSet, rset)
import Data.RecordLike.RSingleton (class RSingleton, rsingleton)
import Data.RecordLike.RUnion (class RUnion, runion)

--   , class RRenameMany
--   , rrenameMany
-- renameFields
--   :: forall f l0 l1 l2 r0 r1 r2
--    . Reify l0
--   => RenameFields l0 l1 l2
--   => RowToList r0 l0
--   => ListToRow l1 r1
--   => RowToList r2 l2
--   => f r0
--   -> Record r1
--   -> Record r2
