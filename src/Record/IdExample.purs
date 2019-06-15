module Example.Id where

import Prelude (class Functor, Unit, identity)
import Type.Row (class RowToList, Cons, RLProxy(RLProxy), kind RowList)

import Record (get)
import Type.Data.Symbol (class IsSymbol, SProxy(SProxy))
import Type.Row (class Cons)

newtype LBox r v =
  LBox (forall z
          . (forall s r'. Cons s v r' r => IsSymbol s => SProxy s -> z)
         -> z
       )

unLBox
  :: forall r v z
   . (forall s r'. Cons s v r' r => IsSymbol s => SProxy s -> z)
  -> LBox r v
  -> z
unLBox g (LBox f) = f g

readYup :: forall r v. Record r -> LBox r v -> v
readYup rec (LBox f) = f \s -> get s rec

readNope :: forall r v. Record r -> LBox r v -> v
readNope rec = unLBox \s -> get s rec

-- Error found:
-- in module Example.Id
-- at src/Record/IdExample.purs:38:20 - 38:20 (line 38, column 20 - line 38, column 20)
--   Could not match constrained type
--     Cons t3 v0 t4 r1 => IsSymbol t3 => ... ... -> z2
--   with type
--     Cons s5 v0 r'7 r1 => IsSymbol s5 => ... ... -> t9
-- while trying to match type Cons t3 v0 t4 r1 => IsSymbol t3 => ... ... -> z2
--   with type Cons s5 t6 r'7 t8 => IsSymbol s5 => ... ... -> t9
-- while checking that expression f
--   has type (forall s r'. Cons s v0 r' r1 => IsSymbol ... => ...) -> z2
-- in value declaration unLBox'
-- where z2 is a rigid type variable
--         bound at (line 0, column 0 - line 0, column 0)
--       v0 is a rigid type variable
--         bound at (line 0, column 0 - line 0, column 0)
--       r1 is a rigid type variable
--         bound at (line 0, column 0 - line 0, column 0)
--       r'7 is a rigid type variable
--         bound at (line 0, column 0 - line 0, column 0)
--       s5 is a rigid type variable
--         bound at (line 0, column 0 - line 0, column 0)
--       t6 is an unknown type
--       t8 is an unknown type
--       t9 is an unknown type
--       t4 is an unknown type
--       t3 is an unknown type
-- See https://github.com/purescript/documentation/blob/master/errors/ConstrainedTypeUnified.md for more information,
-- or to contribute content related to this error.

-- unLBox'
--   :: forall r v z
--    . LBox r v
--   -> (forall s r'
--          . Cons s v r' r
--         => IsSymbol s
--         => SProxy s -> z
--      )
--   -> z
-- unLBox' (LBox f) = f

yup :: forall r v. LBox r v -> LBox r v
yup (LBox f) = LBox \s -> f s

-- Error found:
-- in module Example.Id
-- at src/Record/IdExample.purs:75:33 - 75:34 (line 75, column 33 - line 75, column 34)
--   Could not match constrained type
--     Cons t3 t0 t4 t1 => IsSymbol t3 => ... ... -> z2
--   with type
--     Cons s5 v10 r'7 r11 => IsSymbol s5 => ... ... -> t9
-- while trying to match type Cons t3 t0 t4 t1 => IsSymbol t3 => ... ... -> z2
--   with type Cons s5 t6 r'7 t8 => IsSymbol s5 => ... ... -> t9
-- while checking that expression f
--   has type (forall s r'. Cons s t0 r' t1 => IsSymbol ... => ...) -> z2
-- in value declaration nopeConstrained
-- where v10 is a rigid type variable
--         bound at (line 0, column 0 - line 0, column 0)
--       r11 is a rigid type variable
--         bound at (line 0, column 0 - line 0, column 0)
--       z2 is a rigid type variable
--         bound at (line 75, column 33 - line 75, column 34)
--       r'7 is a rigid type variable
--         bound at (line 0, column 0 - line 0, column 0)
--       s5 is a rigid type variable
--         bound at (line 0, column 0 - line 0, column 0)
--       t6 is an unknown type
--       t8 is an unknown type
--       t0 is an unknown type
--       t1 is an unknown type
--       t9 is an unknown type
--       t4 is an unknown type
--       t3 is an unknown type
-- See https://github.com/purescript/documentation/blob/master/errors/ConstrainedTypeUnified.md for more information,
-- or to contribute content related to this error.
--
-- -- -- Could not match constrained type
-- nopeConstrained :: forall r v. LBox r v -> LBox r v
-- nopeConstrained (LBox f) = LBox f

-- -- No type class instance was found for
nopeNoInstance :: forall r v. LBox r v -> LBox r v
nopeNoInstance = unLBox \s -> LBox \f -> f s











class GId (l :: RowList) (r :: # Type) | l -> r where
  gId :: RLProxy l -> Record r -> Record r

instance gIdCons :: GId (Cons s (va -> vb) l') r where
  gId l record = record

id :: forall l r. GId l r => RowToList r l => Record r -> Record r
id = gId (RLProxy :: RLProxy l)

x1 = id { a: (identity :: forall a. a -> a) }
x2 = id ({ a: identity } :: { a :: forall a. a -> a })
x3 = id ({ a: (identity :: forall a. a -> a) } :: { a :: forall a. a -> a })
x4 = id { a: (identity :: forall a f. Functor f => f a -> f a) }







-- module Example.Id where
--
-- import Prelude (class Functor, Unit, identity)
-- import Effect (Effect)
-- import Effect.Console (log)
-- import Type.Row (class RowToList, Cons, RLProxy(RLProxy), kind RowList)
--
-- import Record (get)
-- import Type.Data.Symbol (class IsSymbol, SProxy(SProxy))
-- import Type.Row (class Cons)
--
-- newtype LBox r v =
--   LBox (forall z
--           . (forall s r'. Cons s v r' r => IsSymbol s => SProxy s -> z)
--          -> z
--        )
--
-- -- for 0
-- unLBox
--   :: forall r v z
--    . (forall s r'. Cons s v r' r => IsSymbol s => SProxy s -> z)
--   -> LBox r v
--   -> z
-- unLBox g (LBox f) = f g
--
-- -- for 2
-- -- unLBox
-- --   :: forall r v
-- --    . LBox r v
-- --   -> (forall z
-- --        . (forall s r'. Cons s v r' r => IsSymbol s => SProxy s -> z)
-- --       -> z
-- --      )
-- -- unLBox (LBox f) g = f g
--
-- -- for 3 -- doesn't compile b/c natefaubion doesn't permit (forall * forall)
-- -- unLBox :: forall r v. (forall z. (forall s r'. Cons s v r' r => IsSymbol s => SProxy s -> z) -> LBox r v -> z)
-- -- unLBox g (LBox f) = f g
--
-- -- readYup ∷ ∀ r v. Record r → LBox r v → v
-- -- readYup rec (LBox f) = f \s → get s rec
--
-- -- 0
-- readNope :: forall r v. Record r -> LBox r v -> v
-- readNope rec = unLBox \s -> get s rec
--
-- -- 1
-- -- readNope :: forall r v. Record r -> LBox r v -> v
-- -- readNope rec (LBox f) = f \s -> get s rec
--
-- -- 2
-- -- readNope :: forall r v. Record r -> LBox r v -> v
-- -- readNope rec lbox = unLBox lbox \s -> get s rec
--
-- -- unLBox' ∷ ∀ r v z. LBox r v → (∀ s r'. Cons s v r' r ⇒ IsSymbol s ⇒ SProxy s → z) → z
-- -- unLBox' (LBox f) = f
-- --
-- -- yup ∷ ∀ r v. LBox r v -> LBox r v
-- -- yup (LBox f) = LBox \s → f s
-- --
-- -- -- Could not match constrained type
-- -- nopeConstrained ∷ ∀ r v. LBox r v -> LBox r v
-- -- nopeConstrained (LBox f) = LBox f
-- --
-- -- -- No type class instance was found for
-- -- nopeNoInstance ∷ ∀ r v. LBox r v → LBox r v
-- nopeNoInstance = unLBox \s → LBox \f → f s
