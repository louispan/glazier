{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Internal where

import Control.Applicative
import Control.Lens hiding ((<.>))
import Data.Functor.Bind
import Data.Semigroup

------------------------------------------------------------------------------
-- EffectMay
------------------------------------------------------------------------------
-- | Wrap a Maybe monadic effect with a phantom type argument.
-- EffectMay will produce Nothing on empty traversals, but uses the monoid of
-- the result type (inside the Maybe) to mappend traversals.
-- This is so that I can differentiate between failures and sucessful traversals.
newtype EffectMay m r a = EffectMay { getEffectMay :: m (Maybe r) }

instance Functor (EffectMay m r) where
  fmap _ (EffectMay m) = EffectMay m

instance Contravariant (EffectMay m r) where
  contramap _ (EffectMay m) = EffectMay m

instance (Apply m, Semigroup r) => Semigroup (EffectMay m r a) where
  EffectMay ma <> EffectMay mb = EffectMay (liftF2 go ma mb)
    where
      go Nothing b = b
      go a Nothing = a
      go (Just a) (Just b) = Just (a <> b)

instance (Applicative m, Monoid r) => Monoid (EffectMay m r a) where
  mempty = EffectMay (pure Nothing)

  EffectMay ma `mappend` EffectMay mb = EffectMay (liftA2 go ma mb)
    where
      go Nothing b = b
      go a Nothing = a
      go (Just a) (Just b) = Just (mappend a b)

instance (Apply m, Semigroup r) => Apply (EffectMay m r) where
  EffectMay ma <.> EffectMay mb = EffectMay (liftF2 go ma mb)
    where
      go Nothing b = b
      go a Nothing = a
      go (Just a) (Just b) = Just (a <> b)

instance (Applicative m, Monoid r) => Applicative (EffectMay m r) where
  pure _ = EffectMay (pure Nothing)

  EffectMay ma <*> EffectMay mb = EffectMay (liftA2 go ma mb)
    where
      go Nothing b = b
      go a Nothing = a
      go (Just a) (Just b) = Just (mappend a b)
