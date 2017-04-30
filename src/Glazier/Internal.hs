{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Internal where

import Control.Applicative
import Control.Lens hiding ((<.>))
import Control.Lens.Internal.Zoom (Err(..))
import Data.Functor.Bind
import Data.Semigroup

------------------------------------------------------------------------------
-- EffectErr
------------------------------------------------------------------------------

-- | Wrap a Either monadic effect with a phantom type argument.
-- Uses 'Err' which makes a monoid out of Either.
newtype EffectErr e m r a = EffectErr { getEffectErr :: m (Err e r) }

instance Functor (EffectErr e m r) where
  fmap _ (EffectErr m) = EffectErr m
  {-# INLINE fmap #-}

instance Contravariant (EffectErr e m r) where
  contramap _ (EffectErr m) = EffectErr m
  {-# INLINE contramap #-}

instance (Apply m, Semigroup r) => Semigroup (EffectErr e m r a) where
  EffectErr ma <> EffectErr mb = EffectErr (liftF2 (<>) ma mb)
  {-# INLINE (<>) #-}

instance (Applicative m, Monoid r) => Monoid (EffectErr e m r a) where
  mempty = EffectErr (pure mempty)
  {-# INLINE mempty #-}

  EffectErr ma `mappend` EffectErr mb = EffectErr (liftA2 mappend ma mb)
  {-# INLINE mappend #-}

instance (Apply m, Semigroup r) => Apply (EffectErr e m r) where
  EffectErr ma <.> EffectErr mb = EffectErr (liftF2 (<>) ma mb)
  {-# INLINE (<.>) #-}

instance (Applicative m, Monoid r) => Applicative (EffectErr e m r) where
  pure _ = EffectErr (pure mempty)
  {-# INLINE pure #-}

  EffectErr ma <*> EffectErr mb = EffectErr (liftA2 mappend ma mb)
  {-# INLINE (<*>) #-}
