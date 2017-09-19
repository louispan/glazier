{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Glazier.Gate where

import qualified Control.Category as C
import Control.Arrow
import Data.Diverse
import Data.Profunctor
import Data.Proxy
import Data.Semigroup

newtype Gate r a b = Gate { runGate :: (b -> r) -> (a -> r) }

-- | Connect in parallel. Use '>>>' to connect in sequence.
instance Semigroup r => Semigroup (Gate r a b) where
    (Gate k) <> (Gate k') = Gate $ \h a -> k h a <> k' h a

instance Profunctor (Gate r) where
    dimap f g (Gate k) = Gate $ \h a -> k (h . g) (f a)

instance Strong (Gate r) where
    first' (Gate k) = Gate $ \h (a, c) -> k (\b -> h (b, c)) a
    second' (Gate k) = Gate $ \h (c, a) -> k (\b -> h (c, b)) a

instance Choice (Gate r) where
    left' (Gate k) = Gate $ \h a -> case a of
                                        Left a' -> k (h . Left) a'
                                        Right c -> h (Right c)
    right' (Gate k) = Gate $ \h a -> case a of
                                         Right a' -> k (h . Right) a'
                                         Left c -> h (Left c)

instance C.Category (Gate r) where
    id = Gate Prelude.id
    (Gate bc) . (Gate ab) = Gate $ \h a -> ab (bc h) a

instance Arrow (Gate r) where
    arr f = Gate (. f)
    first (Gate k) = Gate $ \h (a, c) -> k (\b -> h (b, c)) a
    second (Gate k) = Gate $ \h (c, a) -> k (\b -> h (c, b)) a

instance ArrowChoice (Gate r) where
    left (Gate k) = Gate $ \h a -> case a of
                                       Left a' -> k (h . Left) a'
                                       Right c -> h (Right c)
    right (Gate k) = Gate $ \h a -> case a of
                                        Right a' -> k (h . Right) a'
                                        Left c -> h (Left c)

-- | Like 'Choice' or 'ArrowChoice' but using 'Which'
instance Faceted (Gate r) where
    faceted (Gate k) = Gate $ \h as -> case trial as of
                                         Left as' -> h (diversify as')
                                         Right a -> k (h . pick) a

-- | Like 'Strong' or 'Arrow' but using 'Many'
instance Itemized (Gate r') where
    itemized
        :: forall r a b as. UniqueMember a as
        => Gate r a b -> Gate r (Many as) (Many (Replace a b as))
    itemized (Gate k) =  Gate $ \h as -> k (h . replace' @a Proxy as) (fetch @a as)

-- | Like 'Faceted' but transforming from 'Which'
instance Injected (Gate r) where
    injected (Gate k) = Gate $ \h as -> case reinterpret as of
                                         Left as' -> h (diversify as')
                                         Right as' -> k (h . diversify) as'

-- | Like 'Itemized' but transforming from 'Many'
instance Projected (Gate r') where
    projected
        :: forall r as as' bs. (Select as as', Amend' as bs as')
        => Gate r (Many as) (Many bs) -> Gate r (Many as') (Many (Replaces as bs as'))
    projected (Gate k) = Gate $ \h as' -> k (h . amend' @as Proxy as') (select @as as')

-- Gate r (Which as) (Which bs)
-- Gate r (Which cs) (Which ds)
-- where some of bs is in cs
-- but some of bs is not in cs
-- I want to connect as much of bs to cs
-- the else out output leftover bs
-- and (optionally) exposee the bits of cs not in @as@
