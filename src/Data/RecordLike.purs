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
  , class RSingleton
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
  , rsingleton
  , runion
  ) where

import Prelude (const, eq, identity, (<<<))

import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (class VariantEqs, Variant)
import Data.Variant (inj, on) as Variant
import Data.Variant.Internal (class VariantTags)
import Record
  ( class EqualFields
  , delete
  , disjointUnion
  , get
  , equal
  , insert
  , merge
  , modify
  , nub
  , rename
  , set
  , union
  ) as Record
import Record.Builder (Builder)
import Record.Builder
  ( delete
  , disjointUnion
  , insert
  , merge
  , modify
  , nub
  , rename
  , union
  ) as Builder
import Record.Extra.Utils (singleton) as Record
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

class RDelete
  (p  :: Type -> Type -> Type)
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
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> g s
    -> p (f r0) (f r1)

instance rdeleteBuilder
  :: IsSymbol s
  => RDelete Builder Record SProxy s l0 r0 l1 r1
  where
  rdelete _ _ = Builder.delete

instance rdeleteRecord
  :: IsSymbol s
  => RDelete Function Record SProxy s l0 r0 l1 r1
  where
  rdelete _ _ = Record.delete

instance rdeleteRProxy
  :: RDelete Function RProxy g s l0 r0 l1 r1
  where
  rdelete _ _ _ _ = RProxy

class RDisjointUnion
  (p  :: Type -> Type -> Type)
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
  rdisjointUnion
    :: Nub r2 r2
    => Union r0 r1 r2
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> TypeRow.RLProxy l2
    -> f r0
    -> p (f r1) (f r2)

instance rdisjointUnionBuilder
  :: RDisjointUnion Builder Record l0 r0 l1 r1 l2 r2
  where
  rdisjointUnion _ _ _ = Builder.disjointUnion

instance rdisjointUnionRecord
  :: RDisjointUnion Function Record l0 r0 l1 r1 l2 r2
  where
  rdisjointUnion _ _ _ = Record.disjointUnion

instance rdisjointUnionRProxy
  :: RDisjointUnion Function RProxy l0 r0 l1 r1 l2 r2
  where
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
  requal :: RowToList r l => TypeRow.RLProxy l -> f r -> f r -> Boolean

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
  rget :: forall r' v . Cons s v r' r => TypeRow.RLProxy l -> g s -> f r -> v

instance rgetRecord :: IsSymbol s => RGet Record SProxy s l r where
  rget _ = Record.get

class RInsert
  (p  :: Type -> Type -> Type)
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
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> g s
    -> v
    -> p (f r0) (f r1)

instance rinsertBuilder
  :: IsSymbol s
  => RInsert Builder Record SProxy s l0 r0 l1 r1
  where
  rinsert _ _ = Builder.insert

instance rinsertRecord
  :: IsSymbol s
  => RInsert Function Record SProxy s l0 r0 l1 r1
  where
  rinsert _ _ = Record.insert

instance rinsertRProxy
  :: RInsert Function RProxy g s l0 r0 l1 r1
  where
  rinsert _ _ _ _ _ = RProxy

instance rinsertVariant
  :: IsSymbol s
  => RInsert Function Variant SProxy s l0 r0 l1 r1
  where
  rinsert _ _ s v _ = Variant.inj s v

class RMerge
  (p  :: Type -> Type -> Type)
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
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> TypeRow.RLProxy l2
    -> f r0
    -> p (f r1) (f r3)

instance rmergeBuilder
  :: Union r1 r0 r2
  => RMerge Builder Record l0 r0 l1 r1 l2 r2
  where
  rmerge _ _ _ = Builder.merge

instance rmergeRecord :: RMerge Function Record l0 r0 l1 r1 l2 r2 where
  rmerge _ _ _ = Record.merge

instance rmergeRProxy :: RMerge Function RProxy l0 r0 l1 r1 l2 r2 where
  rmerge _ _ _ _ _ = RProxy

class RModify
  (p  :: Type -> Type -> Type)
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
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> g s
    -> (v0 -> v1)
    -> p (f r0) (f r1)

instance rmodifyBuilder
  :: IsSymbol s
  => RModify Builder Record SProxy s l0 r0 l1 r1
  where
  rmodify _ _ = Builder.modify

instance rmodifyRecord
  :: IsSymbol s
  => RModify Function Record SProxy s l0 r0 l1 r1
  where
  rmodify _ _ = Record.modify

instance rmodifyRProxy :: RModify Function RProxy g s l0 r0 l1 r1 where
  rmodify _ _ _ _ _ = RProxy

instance rmodifyVariant
  ::  IsSymbol s
  => RModify Function Variant SProxy s l0 r0 l1 r1
  where
  rmodify _ _ s f =
    Variant.on
      s
      (Variant.inj s <<< f)
      (unsafeCoerce <<< identity)

-- class RInsert
--   (p  :: Type -> Type -> Type)
--   (f  :: # Type -> Type)
--   (g  :: Symbol -> Type)
--   (s  :: Symbol)
--   (l0 :: RowList)
--   (r0 :: # Type)
--   (l1 :: RowList)
--   (r1 :: # Type)
--   | l0 -> r0
--   , l1 -> r1
--   where
--   rinsert
--     :: forall v
--      . Cons s v r0 r1
--     => Lacks s r0
--     => TypeRow.RLProxy l0
--     -> TypeRow.RLProxy l1
--     -> g s
--     -> v
--     -> p (f r0) (f r1)
-- class RProject
--   (p  :: Type -> Type -> Type)
--   (f  :: # Type -> Type)
--   (g  :: Symbol -> Type)
--   (s  :: Symbol)
--   (l :: RowList)
--   (r :: # Type)
--   | l -> r
--   where
--   rproject
--     :: forall h r' v
--      . R.Cons s v r' r
--     -- => IsSymbol s
--     => Alternative h
--     => g sym
--     -> f r
--     -> h a
-- instance rprojectVariant :: RProject p Variant g s l0 r0 l1 r1

class RNub
  (p  :: Type -> Type -> Type)
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
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> p (f r0) (f r1)

instance rnubBuilder :: RNub Builder Record l0 r0 l1 r1 where
  rnub _ _ = Builder.nub

instance rnubRecord :: RNub Function Record l0 r0 l1 r1 where
  rnub _ _ = Record.nub

instance rnubRProxy :: RNub Function RProxy l0 r0 l1 r1 where
  rnub _ _ _ = RProxy

instance rnubVariant :: RNub Function Variant l0 r0 l1 r1 where
  rnub _ _ = unsafeCoerce

class RRename
  (p  :: Type -> Type -> Type)
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
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> g s0
    -> g s1
    -> p (f r0) (f r1)

instance rrenameBuilder
  :: ( IsSymbol s0
     , IsSymbol s1
     )
  => RRename Builder Record SProxy s0 s1 l0 r0 l1 r1 where
  rrename _ _ = Builder.rename

instance rrenameRecord
  :: ( IsSymbol s0
     , IsSymbol s1
     )
  => RRename Function Record SProxy s0 s1 l0 r0 l1 r1 where
  rrename _ _ = Record.rename

instance rrenameRProxy :: RRename Function RProxy g s0 s1 l0 r0 l1 r1 where
  rrename _ _ _ _ _ = RProxy

instance rrenameVariant
  :: ( IsSymbol s0
     , IsSymbol s1
     )
  => RRename Function Variant SProxy s0 s1 l0 r0 l1 r1 where
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
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
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
  rsingleton = Record.singleton

class RUnion
  (p  :: Type -> Type -> Type)
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
    => TypeRow.RLProxy l0
    -> TypeRow.RLProxy l1
    -> TypeRow.RLProxy l2
    -> f r0
    -> p (f r1) (f r2)

instance runionBuilder
  :: Union r1 r0 r2
  => RUnion Builder Record l0 r0 l1 r1 l2 r2
  where
  runion _ _ _ = Builder.union

instance runionRecord :: RUnion Function Record l0 r0 l1 r1 l2 r2 where
  runion _ _ _ = Record.union

instance runionRProxy :: RUnion Function RProxy l0 r0 l1 r1 l2 r2 where
  runion _ _ _ _ _ = RProxy
