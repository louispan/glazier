{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Control.Arrow.Gate where

import qualified Control.Category as C
import Control.Arrow
import Data.Diverse
import Data.Profunctor
import Data.Proxy
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

 Extra BIn   ┌-------------------┐
 not in AOut |           ┌----┐  |
 ------------|-----------|    |  |
             |           | B  |  | BOut
             |  ┌----┐   |    |--|-----------
             |  |    |---|    |  |
 ------------|--| A  |   └----┘  |
 AIn         |  |    |-----------|-----------
             |  |    |           | Extra AOut
             |  └----┘           | not in BIn
             └-------------------┘


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
newtype Gate r a b = Gate
    { runGate :: (b -> r) -- given handlers for outputs
              -> (a -> r) -- return handler for inputs
    }

-- | Used to ignore certain inputs
filterInput :: Monoid r => (a' -> Maybe a) -> Gate r a b -> Gate r a' b
filterInput f (Gate k) = Gate $ \h a -> case f a of
    Nothing -> mempty
    Just a' -> k h a'

-- | Used to ignore certain outputs
filterOutput :: Monoid r => (b -> Maybe b') -> Gate r a b -> Gate r a b'
filterOutput g (Gate k) = Gate $ \h a -> k (go h) a
  where
    go h b = case g b of
        Nothing -> mempty
        Just b' -> h b'

-- | Connect in parallel. Input is feed to both gates and the output of
-- both gates are '<>' together.
instance Semigroup r => Semigroup (Gate r a b) where
    (Gate k) <> (Gate k') = Gate $ \h a -> k h a <> k' h a

-- | You can contramap the input and fmap the output of a 'Gate'.
instance Profunctor (Gate r) where
    dimap f g (Gate k) = Gate $ \h a -> k (h . g) (f a)

-- | Additional annotation on inputs are just copied to the output.
instance Strong (Gate r) where
    first' (Gate k) = Gate $ \h (a, c) -> k (\b -> h (b, c)) a
    second' (Gate k) = Gate $ \h (c, a) -> k (\b -> h (c, b)) a

-- | Any 'Gate' can be made into a alternative gate which only used when the input type match
-- else the input is feed directly to the output.
instance Choice (Gate r) where
    left' (Gate k) = Gate $ \h a -> case a of
                                        Left a' -> k (h . Left) a' -- run through provided gate
                                        Right c -> h (Right c) -- else feed input directly to output
    right' (Gate k) = Gate $ \h a -> case a of
                                         Right a' -> k (h . Right) a' -- run through provided gate
                                         Left c -> h (Left c) -- else feed input directly to output

-- | A Gate is a category, which means you can use '>>>' to connect 'Gates' serially.
instance C.Category (Gate r) where
    id = Gate Prelude.id
    (Gate bc) . (Gate ab) = Gate $ \h a -> ab (bc h) a

-- | Additional annotation on inputs are just copied to the output.
instance Arrow (Gate r) where
    arr f = Gate (. f)
    first (Gate k) = Gate $ \h (a, c) -> k (\b -> h (b, c)) a -- copy annotation to output
    second (Gate k) = Gate $ \h (c, a) -> k (\b -> h (c, b)) a -- copy annotation to output

-- | Any 'Gate' can be made into a alternative gate which only used when the input type match
-- else the input is feed directly to the output.
instance ArrowChoice (Gate r) where
    left (Gate k) = Gate $ \h a -> case a of
                                       Left a' -> k (h . Left) a' -- run through provided gate
                                       Right c -> h (Right c) -- else feed input directly to output
    right (Gate k) = Gate $ \h a -> case a of
                                        Right a' -> k (h . Right) a' -- run through provided gate
                                        Left c -> h (Left c) -- else feed input directly to output

-- | Like 'Choice' or 'ArrowChoice' but using 'Which'
instance Faceted (Gate r) where
    faceted (Gate k) = Gate $ \h as -> case trial as of
                                         Right a -> k (h . pick) a -- run through provided gate
                                         Left as' -> h (diversify as') -- else feed input directly to output

-- | Like 'Strong' or 'Arrow' but using 'Many'
instance Itemized (Gate r') where
    itemized
        :: forall r a b as. UniqueMember a as
        => Gate r a b -> Gate r (Many as) (Many (Replace a b as))
    itemized (Gate k) =  Gate $ \h as -> k (h . replace' @a Proxy as) (fetch @a as) -- copy annotation to output

-- | Like 'Faceted' but transforming from 'Which'
instance Injected (Gate r) where
    injected (Gate k) = Gate $ \h as -> case reinterpret as of
                                         Right as' -> k (h . diversify) as'-- run through provided gate
                                         Left as' -> h (diversify as') -- else feed input directly to output

-- | Like 'Itemized' but transforming from 'Many'
instance Projected (Gate r') where
    projected
        :: forall r as as' bs. (Select as as', Amend' as bs as')
        => Gate r (Many as) (Many bs) -> Gate r (Many as') (Many (Replaces as bs as'))
    projected (Gate k) = Gate $ \h as' -> k (h . amend' @as Proxy as') (select @as as') -- copy annotation to output
