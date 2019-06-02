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
  --, get
  --, equal
  , insert
  , merge
  , modify
  , nub
  , rename
  --, set
  , union
  ) as Builder
import Record.Extra.Utils (singleton) as Record
import Type.Data.RowList (RLProxy) -- Argonaut dependency
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
    => RLProxy l0
    -> RLProxy l1
    -> g s
    -> p (f r0) (f r1)

instance rdeleteRecord
  :: IsSymbol s
  => RDelete Function Record SProxy s l0 r0 l1 r1
  where
  rdelete _ _ = Record.delete

instance rdeleteRProxy
  :: RDelete Function RProxy g s l0 r0 l1 r1
  where
  rdelete _ _ _ _ = RProxy

instance rdeleteBuilder
  :: IsSymbol s
  => RDelete Builder Record SProxy s l0 r0 l1 r1
  where
  rdelete _ _ = Builder.delete

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
    => RLProxy l0
    -> RLProxy l1
    -> RLProxy l2
    -> f r0
    -> p (f r1) (f r2)

instance rdisjointUnionRecord
  :: RDisjointUnion Function Record l0 r0 l1 r1 l2 r2
  where
  rdisjointUnion _ _ _ = Record.disjointUnion

instance rdisjointUnionRProxy
  :: RDisjointUnion Function RProxy l0 r0 l1 r1 l2 r2
  where
  rdisjointUnion _ _ _ _ _= RProxy

instance rdisjointUnionBuilder
  :: RDisjointUnion Builder Record l0 r0 l1 r1 l2 r2
  where
  rdisjointUnion _ _ _ = Builder.disjointUnion

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
  requal :: RowToList r l => RLProxy l -> f r -> f r -> Boolean

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
  rget :: forall r' v . Cons s v r' r => RLProxy l -> g s -> f r -> v

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
    => RLProxy l0
    -> RLProxy l1
    -> g s
    -> v
    -> p (f r0) (f r1)

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

instance rinsertBuilder
  :: IsSymbol s
  => RInsert Builder Record SProxy s l0 r0 l1 r1
  where
  rinsert _ _ = Builder.insert

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
    => RLProxy l0
    -> RLProxy l1
    -> g s
    -> (v0 -> v1)
    -> p (f r0) (f r1)

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

instance rmodifyBuilder
  :: IsSymbol s
  => RModify Builder Record SProxy s l0 r0 l1 r1
  where
  rmodify _ _ = Builder.modify

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
    => RLProxy l0
    -> RLProxy l1
    -> p (f r0) (f r1)

instance rnubRecord :: RNub Function Record l0 r0 l1 r1 where
  rnub _ _ = Record.nub

instance rnubRProxy :: RNub Function RProxy l0 r0 l1 r1 where
  rnub _ _ _ = RProxy

instance rnubVariant :: RNub Function Variant l0 r0 l1 r1 where
  rnub _ _ = unsafeCoerce

instance rnubBuilder :: RNub Builder Record l0 r0 l1 r1 where
  rnub _ _ = Builder.nub

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
    => RLProxy l0
    -> RLProxy l1
    -> RLProxy l2
    -> f r0
    -> p (f r1) (f r3)

instance rmergeRecord :: RMerge Function Record l0 r0 l1 r1 l2 r2 where
  rmerge _ _ _ = Record.merge

instance rmergeRProxy :: RMerge Function RProxy l0 r0 l1 r1 l2 r2 where
  rmerge _ _ _ _ _ = RProxy

instance rmergeBuilder
  :: Union r1 r0 r2
  => RMerge Builder Record l0 r0 l1 r1 l2 r2
  where
  rmerge _ _ _ = Builder.merge

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
    => RLProxy l0
    -> RLProxy l1
    -> g s0
    -> g s1
    -> p (f r0) (f r1)

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

instance rrenameBuilder
  :: ( IsSymbol s0
     , IsSymbol s1
     )
  => RRename Builder Record SProxy s0 s1 l0 r0 l1 r1 where
  rrename _ _ = Builder.rename

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
    => RLProxy l0
    -> RLProxy l1
    -> RLProxy l2
    -> f r0
    -> p (f r1) (f r2)

instance runionRecord :: RUnion Function Record l0 r0 l1 r1 l2 r2 where
  runion _ _ _ = Record.union

instance runionRProxy :: RUnion Function RProxy l0 r0 l1 r1 l2 r2 where
  runion _ _ _ _ _ = RProxy

instance runionBuilder
  :: Union r1 r0 r2
  => RUnion Builder Record l0 r0 l1 r1 l2 r2
  where
  runion _ _ _ = Builder.union
































-- module Record.Builder
--   ( Builder
--   , build
--   , insert
--   , modify
--   , delete
--   , rename
--   , merge
--   , union
--   , disjointUnion
--   , nub
--   ) where
--
-- import Prelude
--
-- import Data.Function.Uncurried (runFn2)
-- import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
-- import Record.Unsafe.Union (unsafeUnionFn)
-- import Type.Row as Row
-- import Unsafe.Coerce (unsafeCoerce)
--
-- foreign import copyRecord :: forall r1. Record r1 -> Record r1
-- foreign import unsafeInsert :: forall a r1 r2. String -> a -> Record r1 -> Record r2
-- foreign import unsafeModify :: forall a b r1 r2. String -> (a -> b) -> Record r1 -> Record r2
-- foreign import unsafeDelete :: forall r1 r2. String -> Record r1 -> Record r2
-- foreign import unsafeRename :: forall r1 r2. String -> String -> Record r1 -> Record r2
--
-- -- | A `Builder` can be used to `build` a record by incrementally adding
-- -- | fields in-place, instead of using `insert` and repeatedly generating new
-- -- | immutable records which need to be garbage collected.
-- -- |
-- -- | The mutations accumulated in a `Builder` are safe because intermediate states can't be
-- -- | observed. These mutations, then, are performed all-at-once in the `build` function.
-- -- |
-- -- | The `Category` instance for `Builder` can be used to compose builders.
-- -- |
-- -- | For example:
-- -- |
-- -- | ```purescript
-- -- | build (insert x 42 >>> insert y "testing") {} :: { x :: Int, y :: String }
-- -- | ```
-- newtype Builder a b = Builder (a -> b)
--
-- -- | Build a record, starting from some other record.
-- build :: forall r1 r2. Builder (Record r1) (Record r2) -> Record r1 -> Record r2
-- build (Builder b) r1 = b (copyRecord r1)
--
-- derive newtype instance semigroupoidBuilder :: Semigroupoid Builder
-- derive newtype instance categoryBuilder :: Category Builder
--
-- -- | Build by inserting a new field.
-- insert
--   :: forall l a r1 r2
--    . Row.Cons l a r1 r2
--   => Row.Lacks l r1
--   => IsSymbol l
--   => SProxy l
--   -> a
--   -> Builder (Record r1) (Record r2)
-- insert l a = Builder \r1 -> unsafeInsert (reflectSymbol l) a r1
--
-- -- | Build by modifying an existing field.
-- modify
--   :: forall l a b r r1 r2
--    . Row.Cons l a r r1
--   => Row.Cons l b r r2
--   => IsSymbol l
--   => SProxy l
--   -> (a -> b)
--   -> Builder (Record r1) (Record r2)
-- modify l f = Builder \r1 -> unsafeModify (reflectSymbol l) f r1
--
-- -- | Build by deleting an existing field.
-- delete
--   :: forall l a r1 r2
--    . IsSymbol l
--    => Row.Lacks l r1
--    => Row.Cons l a r1 r2
--    => SProxy l
--    -> Builder (Record r2) (Record r1)
-- delete l = Builder \r2 -> unsafeDelete (reflectSymbol l) r2
--
-- -- | Build by renaming an existing field.
-- rename :: forall l1 l2 a r1 r2 r3
--    . IsSymbol l1
--   => IsSymbol l2
--   => Row.Cons l1 a r2 r1
--   => Row.Lacks l1 r2
--   => Row.Cons l2 a r2 r3
--   => Row.Lacks l2 r2
--   => SProxy l1
--   -> SProxy l2
--   -> Builder (Record r1) (Record r3)
-- rename l1 l2 = Builder \r1 -> unsafeRename (reflectSymbol l1) (reflectSymbol l2) r1
--
-- -- | Build by merging existing fields from another record.
-- merge
--   :: forall r1 r2 r3 r4
--    . Row.Union r1 r2 r3
--   => Row.Nub r3 r4
--   => Record r2
--   -> Builder (Record r1) (Record r4)
-- merge r2 = Builder \r1 -> runFn2 unsafeUnionFn r1 r2
--
-- -- | Build by merging existing fields from another record. Unlike `merge`,
-- -- | this does not remove duplicate labels from the resulting record type.
-- -- | This can result in better inference for some pipelines, deferring the
-- -- | need for a `Nub` constraint.
-- union
--   :: forall r1 r2 r3
--    . Row.Union r1 r2 r3
--   => Record r2
--   -> Builder (Record r1) (Record r3)
-- union r2 = Builder \r1 -> runFn2 unsafeUnionFn r1 r2
--
-- -- | Build by merging some disjoint set of fields from another record.
-- disjointUnion
--   :: forall r1 r2 r3
--    . Row.Union r1 r2 r3
--   => Row.Nub r3 r3
--   => Record r1
--   -> Builder (Record r2) (Record r3)
-- disjointUnion r1 = Builder \r2 -> runFn2 unsafeUnionFn r1 r2
--
-- -- | A coercion which removes duplicate labels from a record's type.
-- nub
--   :: forall r1 r2
--    . Row.Nub r1 r2
--   => Builder (Record r1) (Record r2)
-- nub = Builder unsafeCoerce
