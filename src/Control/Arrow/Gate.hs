{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Arrow.Gate where

-- import Control.Applicative
import Control.Arrow
import qualified Control.Category as C
-- import Data.Diverse.Profunctor
import Data.Profunctor
import Data.Semigroup

{- | 'Gate' is a continuation where given a continuation of an output handler, it will return a input handler.

Mememonic: A digital circuit gate with inputs and outputs.

Connecting in serial:

If output of A matched input of B, then '>>>' is sufficient.

If output of A is strictly smaller then input of B, then we can expand A (faceted A) to pass through
the extra inputs of B, then use '>>>'.
C will take (inputs of A + extra inputs of B), and put (outputs of B)

If output of A is strictly larger then input of B, then we can expand B (faceted B) to pass through
the extra outputs of A, then use '>>>'.
C will take (inputs of A), and put (outputs of B + extra outputs of A)

Both methods can be combined to a C that take (inputs of A + extra inputs of B not in output of A)
and put (outputs of B + extra outputs of A not in input of B)

             ┌------------------------------┐ Extra AOut
 AIn         |  ┌----┐                      | not in BIn
 ------------|--|    |----------------------|-----------
             |  | A  |              ┌----┐  |
             |  |    | Part of AOut |    |  |
             |  |    | that matches |    |  |
             |  |    | part of BOut |    |  |
 Extra BIn   |  |    |--------------|    |  |
 not in AOut |  └----┘              | B  |  | Bout
 ------------|----------------------|    |--|-----------
             |                      └----┘  |
             └------------------------------┘

Ignore input wiring:

Input of B can always be grounded/ignored (so that input will never be fired) by diversifying the output of A
(rmap diverify A), then use '>>>'.
Alternatively use 'ignoreInput' on B, then use '>>>', but this requres r to be a Monoid.
C will take (inputs of A), and put (outputs of B)


Connecting in parallel:
Use (filterInput trial') on A to ignore BIn, and (filterInput trial') on B to ignore AIn
and (rmap diversify) on A to expand AOut to include BOut
and (rmap diversify) on B to expand BOut to include AOut
and <> the result
C will take (input of A and B) and put (output of A and B)


             ┌-------------------┐
 AOut        |           ┌----┐  |
 ------------|-----------|    |  |
             |           | B  |  | BOut
             |  ┌----┐   |    |--|-----------
             |  |    |   |    |  |
 ------------|--| A  |   └----┘  |
 AIn         |  |    |-----------|-----------
             |  |    |           | AOut
             |  └----┘           |
             └-------------------┘

-}

-- | 'Gate' is a profunctor version of ContT monad
-- where given an output handler,
-- it will return a input handler.
-- Mememonic: A digital circuit gate with inputs and outputs.
newtype Gate r a b = Gate
    { runGate :: (b -> r) -- given handlers for outputs
              -> (a -> r) -- return handler for inputs
    }

-- Given isomorphism to @r@ to @r'@, change to result of Gate to @r'@
exchange :: (r -> r') -> (r' -> r) -> Gate r a b -> Gate r' a b
exchange f g (Gate ab) = Gate $ \k a -> f (ab (g . k) a)

pinned :: r -> Gate r a b
pinned r = Gate $ \_ _ -> r

-- | Run both gates and combine the output
meld :: (r -> r -> r) -> Gate r a b -> Gate r a b -> Gate r a b
meld go (Gate f) (Gate g) = Gate $ \k a -> f k a `go` g k a

-- | Used to ignore certain inputs.
-- Ie, pretend to handle more inputs.
suppressInput :: r -> (a -> Maybe a') -> Gate r a' b -> Gate r a b
suppressInput r f (Gate g) = Gate $ \k a -> case f a of
    Nothing -> r
    Just a' -> g k a'

-- | Used to ignore certain outputs
filterOutput :: r -> (b -> Maybe b') -> Gate r a b -> Gate r a b'
filterOutput r f (Gate g) = Gate $ \k a ->
    let go b = case f b of
                Nothing -> r
                Just b' -> k b'
    in g go a

instance Functor (Gate r a) where
    fmap f (Gate ab) = Gate $ \k a -> ab (k . f) a

instance Applicative (Gate r a) where
    pure r = Gate $ \k _ -> k r
    (Gate gab) <*> (Gate ga) = Gate $ \kb x -> flip gab x $ \ab -> ga (kb . ab) x

instance Monad (Gate r a) where
    (Gate ga) >>= f = Gate $ \kb x -> flip ga x $ \a ->
        let Gate gb = f a
        in gb kb x

-- | Connect in parallel. Input is feed to both gates and the output of
-- both gates are '<>' together.
instance Semigroup r => Semigroup (Gate r a b) where
    (Gate f) <> (Gate g) = Gate $ \k a -> f k a <> g k a

instance Monoid r => Monoid (Gate r a b) where
    mempty = pinned mempty
    f `mappend` g = meld mappend f g

-- | You can contramap the input and fmap the output of a 'Gate'.
instance Profunctor (Gate r) where
    dimap f g (Gate ab) = Gate $ \k a -> ab (k . g) (f a)

-- | Additional annotation on inputs are just copied to the output.
instance Strong (Gate r) where
    first' (Gate ab) = Gate $ \k (a, c) -> ab (\b -> k (b, c)) a
    second' (Gate ab) = Gate $ \k (c, a) -> ab (\b -> k (c, b)) a

-- | Any 'Gate' can be made into a alternative gate which only used when the input type match
-- else the input is feed directly to the output.
instance Choice (Gate r) where
    left' (Gate ab) = Gate $ \k a -> case a of
                                        Left a' -> ab (k . Left) a' -- run through provided gate
                                        Right c -> k (Right c) -- else feed input directly to output
    right' (Gate ab) = Gate $ \k a -> case a of
                                         Right a' -> ab (k . Right) a' -- run through provided gate
                                         Left c -> k (Left c) -- else feed input directly to output

-- | A Gate is a category, which means you can use '>>>' to connect 'Gates' serially.
instance C.Category (Gate r) where
    id = Gate Prelude.id
    (Gate bc) . (Gate ab) = Gate $ \k a -> ab (bc k) a

-- | Additional annotation on inputs are just copied to the output.
instance Arrow (Gate r) where
    arr f = Gate (. f)
    first = first'
    second = second'

-- | Any 'Gate' can be made into a alternative gate which only used when the input type match
-- else the input is feed directly to the output.
instance ArrowChoice (Gate r) where
    left = left'
    right = right'

-- newtype PGate r ab = PGate
--     { runPGate :: Gate r (P.At0 ab) (P.At1 ab)
--     }

-- type instance P.PNullary (PGate r) (a, b) = Gate r a b

-- -- | NB. This is also identity for 'Data.Diverse.Profunctor.+||+'
-- instance Monoid r => P.PMEmpty (PGate r) (Which '[], Which '[]) where
--     pmempty = Gate $ \_ _ -> mempty

-- -- | type restricted version of 'P.pmempty' for 'Gate'
-- nulGate :: Monoid r => Gate r (Which '[]) (Which '[])
-- nulGate = P.pmempty

-- -- | Undecidableinstances!
-- instance (ChooseBetween a1 a2 a3 b1 b2 b3
--          ) =>
--          P.PSemigroup (PGate r)
--               (Which a1, Which b1)
--               (Which a2, Which b2)
--               (Which a3, Which b3) where
--     x `pmappend` y = x +||+ y

-- -- | type restricted version of 'P.pmappend' for 'Gate'
-- orGate ::
--     (ChooseBetween a1 a2 a3 b1 b2 b3
--     )
--     => Gate r (Which a1) (Which b1)
--     -> Gate r (Which a2) (Which b2)
--     -> Gate r (Which a3) (Which b3)
-- orGate = P.pmappend
-- infixr 2 `orGate` -- like +++

-- -- | type restricted version of 'P.pmappend' for 'Gate'
-- andGate ::
--     ( ChooseBoth b1 b2 b3
--     )
--     => (r -> r -> r)
--     -> Gate r a (Which b1)
--     -> Gate r a (Which b2)
--     -> Gate r a (Which b3)
-- andGate f x y = meld f (rmap diversify x) (rmap diversify y)
-- infixr 2 `andGate` -- like +++
