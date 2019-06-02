module Data.RecordLike
  ( class RDelete
  , class RDisjointUnion
  , class REmpty
  , class REqual
  , class RGet
  , class RInsert
  , class RMerge
  , class RModify
  , class RNub
  , class RRename
  , class RSet
  , class RUnion
  , rget
  , rdelete
  , rdisjointUnion
  , rempty
  , requal
  , rinsert
  , rmerge
  , rmodify
  , rnub
  , rrename
  , rset
  , runion
  ) where

import Prelude (const, eq, identity, (<<<))

import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (class VariantEqs, Variant)
import Data.Variant (inj, on) as Variant
import Data.Variant.Internal (class VariantTags)
import Record
  ( class EqualFields
  , get
  , set
  , modify
  , insert
  , delete
  , rename
  , equal
  , merge
  , union
  , disjointUnion
  , nub
  ) as Record
import Type.Data.RowList (RLProxy) -- Argonaut dependency
import Type.Row
  ( class Cons
  , class Lacks
  , class Nub
  , class RowToList
  , class Union
  , RProxy(RProxy)
  , kind RowList
  )
import Unsafe.Coerce (unsafeCoerce)

class RDelete
  (f  :: # Type -> Type)
  (g  :: Symbol -> Type)
  (s  :: Symbol)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rdelete
    :: forall v
     . Cons s v r1 r0
    => Lacks s r1
    => RLProxy l0
    -> RLProxy l1
    -> g s
    -> f r0
    -> f r1

instance rdeleteRecord
  :: IsSymbol s
  => RDelete Record SProxy s l0 r0 l1 r1
  where
  rdelete _ _ = Record.delete

instance rdeleteRProxy
  :: RDelete RProxy g s l0 r0 l1 r1
  where
  rdelete _ _ _ _ = RProxy

class RDisjointUnion
  (f :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l2 :: RowList)
  (r2 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l2 -> r2
  where
  rdisjointUnion
    :: Nub r2 r2
    => Union r0 r1 r2
    => RLProxy l0
    -> RLProxy l1
    -> RLProxy l2
    -> f r0
    -> f r1
    -> f r2

instance rdisjointUnionRecord :: RDisjointUnion Record l0 r0 l1 r1 l2 r2 where
  rdisjointUnion _ _ _ = Record.disjointUnion

instance rdisjointUnionRProxy :: RDisjointUnion RProxy l0 r0 l1 r1 l2 r2 where
  rdisjointUnion _ _ _ _ _= RProxy

class REmpty (f :: # Type -> Type) where
  rempty :: f ()

instance remptyRecord :: REmpty Record where
  rempty = {}

instance remptyRProxy :: REmpty RProxy where
  rempty = RProxy

class REqual
  (f :: # Type -> Type)
  (l :: RowList)
  (r :: # Type)
  | l -> r
  where
  requal
    :: RowToList r l
    => RLProxy l
    -> f r
    -> f r
    -> Boolean

instance requalRecord :: Record.EqualFields l r => REqual Record l r where
  requal _ = Record.equal

instance requalRProxy :: REqual RProxy l r where
  requal _ _ _ = true

instance requalVariant
  :: ( VariantEqs l
     , VariantTags l
     )
  => REqual Variant l r where
  requal _ = eq

class RGet
  (f :: # Type -> Type)
  (g :: Symbol -> Type)
  (s :: Symbol)
  (l :: RowList)
  (r :: # Type)
  | l -> r
  where
  rget
    :: forall r' v
     . Cons s v r' r
    => RLProxy l
    -> g s
    -> f r
    -> v

instance rgetRecord :: IsSymbol s => RGet Record SProxy s l r where
  rget _ = Record.get

class RInsert
  (f  :: # Type -> Type)
  (g  :: Symbol -> Type)
  (s  :: Symbol)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rinsert
    :: forall v
    . Cons s v r0 r1
    => Lacks s r0
    => RLProxy l0
    -> RLProxy l1
    -> g s
    -> v
    -> f r0
    -> f r1

instance rinsertRecord
  :: IsSymbol s
  => RInsert Record SProxy s l0 r0 l1 r1
  where
  rinsert _ _ = Record.insert

instance rinsertRProxy :: RInsert RProxy g s l0 r0 l1 r1 where
  rinsert _ _ _ _ _ = RProxy

instance rinsertVariant
  :: IsSymbol s
  => RInsert Variant SProxy s l0 r0 l1 r1
  where
  rinsert _ _ s v _ = Variant.inj s v

class RModify
  (f  :: # Type -> Type)
  (g  :: Symbol -> Type)
  (s  :: Symbol)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rmodify
    :: forall r v0 v1
     . Cons s v0 r r0
    => Cons s v1 r r1
    => RLProxy l0
    -> RLProxy l1
    -> g s
    -> (v0 -> v1)
    -> f r0
    -> f r1

instance rmodifyRecord
  :: IsSymbol s
  => RModify Record SProxy s l0 r0 l1 r1
  where
  rmodify _ _ = Record.modify

instance rmodifyRProxy :: RModify RProxy g s l0 r0 l1 r1 where
  rmodify _ _ _ _ _ = RProxy

instance rmodifyVariant
  ::  IsSymbol s
  => RModify Variant SProxy s l0 r0 l1 r1
  where
  rmodify _ _ s f =
    Variant.on
      s
      (Variant.inj s <<< f)
      (unsafeCoerce <<< identity)

class RNub
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rnub
    :: Nub r0 r1
    => RLProxy l0
    -> RLProxy l1
    -> f r0
    -> f r1

instance rnubRecord :: RNub Record l0 r0 l1 r1 where
  rnub _ _ = Record.nub

instance rnubRProxy :: RNub RProxy l0 r0 l1 r1 where
  rnub _ _ _ = RProxy

instance rnubVariant :: RNub Variant l0 r0 l1 r1 where
  rnub _ _ = unsafeCoerce

class RMerge
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l2 :: RowList)
  (r2 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l2 -> r2
  where
  rmerge
    :: forall r3
     . Nub r2 r3
    => Union r0 r1 r2
    => RLProxy l0
    -> RLProxy l1
    -> RLProxy l2
    -> f r0
    -> f r1
    -> f r3

instance rmergeRecord :: RMerge Record l0 r0 l1 r1 l2 r2 where
  rmerge _ _ _ = Record.merge

instance rmergeRProxy :: RMerge RProxy l0 r0 l1 r1 l2 r2 where
  rmerge _ _ _ _ _ = RProxy

class RRename
  (f  :: # Type -> Type)
  (g  :: Symbol -> Type)
  (s0 :: Symbol)
  (s1 :: Symbol)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rrename
    :: forall r v
     . Cons s0 v r r0
    => Cons s1 v r r1
    => Lacks s0 r
    => Lacks s0 r1
    => Lacks s1 r
    => Lacks s1 r0
    => RLProxy l0
    -> RLProxy l1
    -> g s0
    -> g s1
    -> f r0
    -> f r1

instance rrenameRecord
  :: ( IsSymbol s0
     , IsSymbol s1
     )
  => RRename Record SProxy s0 s1 l0 r0 l1 r1 where
  rrename _ _ = Record.rename

instance rrenameRProxy :: RRename RProxy g s0 s1 l0 r0 l1 r1 where
  rrename _ _ _ _ _ = RProxy

instance rrenameVariant
  :: ( IsSymbol s0
     , IsSymbol s1
     )
  => RRename Variant SProxy s0 s1 l0 r0 l1 r1 where
  rrename _ _ s0 s1 =
    Variant.on
      s0
      (Variant.inj s1)
      unsafeCoerce

class RSet
  (f :: # Type -> Type)
  (g :: Symbol -> Type)
  (s :: Symbol)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  | l0 -> r0
  , l1 -> r1
  where
  rset
    :: forall r v0 v1
     . Cons s v0 r r0
    => Cons s v1 r r1
    => RLProxy l0
    -> RLProxy l1
    -> g s
    -> v1
    -> f r0
    -> f r1

instance rsetRecord :: IsSymbol s => RSet Record SProxy s l0 r0 l1 r1 where
  rset _ _ = Record.set

instance rsetRProxy :: RSet RProxy g s l0 r0 l1 r1 where
  rset _ _ _ _ _ = RProxy

instance rsetVariant :: IsSymbol s => RSet Variant SProxy s l0 r0 l1 r1 where
  rset _ _ s v =
    Variant.on
      s
      (Variant.inj s <<< const v)
      unsafeCoerce

class RUnion
  (f  :: # Type -> Type)
  (l0 :: RowList)
  (r0 :: # Type)
  (l1 :: RowList)
  (r1 :: # Type)
  (l2 :: RowList)
  (r2 :: # Type)
  | l0 -> r0
  , l1 -> r1
  , l2 -> r2
  where
  runion
    :: Union r0 r1 r2
    => RLProxy l0
    -> RLProxy l1
    -> RLProxy l2
    -> f r0
    -> f r1
    -> f r2

instance runionRecord :: RUnion Record l0 r0 l1 r1 l2 r2 where
  runion _ _ _ = Record.union

instance runionRProxy :: RUnion RProxy l0 r0 l1 r1 l2 r2 where
  runion _ _ _ _ _ = RProxy