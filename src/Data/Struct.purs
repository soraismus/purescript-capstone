module Data.Struct
  ( module Data.Struct.RCompare
  , module Data.Struct.RConst
  , module Data.Struct.RContract
  , module Data.Struct.RDelete
  , module Data.Struct.RDisjointUnion
  , module Data.Struct.REmpty
  , module Data.Struct.REqual
  , module Data.Struct.RExpand
  , module Data.Struct.REval
  , module Data.Struct.RGet
  , module Data.Struct.RInsert
  , module Data.Struct.RMatch
  , module Data.Struct.RMerge
  , module Data.Struct.RModify
  , module Data.Struct.RNub
  , module Data.Struct.ROn
  , module Data.Struct.ROnMatch
--   , module Data.Struct.RPick
  , module Data.Struct.RProject
  , module Data.Struct.RRename
  , module Data.Struct.RSet
  , module Data.Struct.RSingleton
  , module Data.Struct.RUnion
  ) where

import Data.Struct.RCompare (class RCompare, rcompare)
import Data.Struct.RConst (class RConst, rconst)
import Data.Struct.RContract (class RContract, rcontract)
import Data.Struct.RDelete (class RDelete, rdelete)
import Data.Struct.RDisjointUnion (class RDisjointUnion, rdisjointUnion)
import Data.Struct.REmpty (class REmpty, rempty)
import Data.Struct.REqual (class REqual, requal)
import Data.Struct.REval (class REval, reval)
import Data.Struct.RExpand (class RExpand, rexpand)
import Data.Struct.RGet (class RGet, rget)
import Data.Struct.RInsert (class RInsert, rinsert)
import Data.Struct.RMatch (class RMatch, rmatch)
import Data.Struct.RMerge (class RMerge, rmerge)
import Data.Struct.RModify (class RModify, rmodify)
import Data.Struct.RNub (class RNub, rnub)
import Data.Struct.ROn (class ROn, ron)
import Data.Struct.ROnMatch (class ROnMatch, ronMatch)
-- import Data.Struct.RPick (class RPick, rpick)
import Data.Struct.RProject (class RProject, rproject)
import Data.Struct.RRename (class RRename, rrename)
import Data.Struct.RSet (class RSet, rset)
import Data.Struct.RSingleton (class RSingleton, rsingleton)
import Data.Struct.RUnion (class RUnion, runion)

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


-- project, project', projectTo, projectTo' instead of pick (or substruct).
