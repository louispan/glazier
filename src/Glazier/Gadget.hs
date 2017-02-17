{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Gadget where

import Control.Applicative
import Control.Lens
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix (MonadFix)
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Semigroup
import Glazier.Class

-- | The Elm update function is @a -> s -> (s, c)@
-- This is isomorphic to @ReaderT a (State s) c@
-- ie, given an action "a", and a current state "s", return the new state "s"
-- and any commands "c" that need to be interpreted externally (eg. download file).
-- This is named Gadget instead of Update to avoid confusion with update from Data.Map
-- NB. This is the same formulation as 'Glaizer.Window'.
-- The only difference is that Gadget has both 'Implant' and 'Dispatch' instances.
newtype GadgetT a s m c = GadgetT
    { runGadgetT :: ReaderT a (StateT s m) c
    } deriving ( MonadState s
               , MonadReader a
               , Monad
               , Applicative
               , Functor
               , Fail.MonadFail
               , Alternative
               , MonadPlus
               , MonadFix
               , MonadIO
               )

makeWrapped ''GadgetT

type Gadget a s = GadgetT a s Identity

_GadgetT :: Iso (GadgetT a s m c) (GadgetT a' s' m' c') (a -> s -> m (c, s)) (a' -> s' -> m' (c', s'))
_GadgetT = _Wrapping GadgetT . iso runReaderT ReaderT . iso (runStateT .) (StateT .)
{-# INLINABLE _GadgetT #-}

-- | Non polymorphic version of _Gadget
_GadgetT' :: Iso' (GadgetT a s m c) (a -> s -> m (c, s))
_GadgetT' = _GadgetT
{-# INLINABLE _GadgetT' #-}

mkGadgetT' :: (a -> s -> m (c, s)) -> GadgetT a s m c
mkGadgetT' = review _GadgetT
{-# INLINABLE mkGadgetT' #-}

runGadgetT' :: GadgetT a s m c -> (a -> s -> m (c, s))
runGadgetT' = view _GadgetT
{-# INLINABLE runGadgetT' #-}

belowGadgetT ::
  ((a -> s -> m (c, s)) -> a' -> s' -> m' (c', s'))
  -> GadgetT a s m c -> GadgetT a' s' m' c'
belowGadgetT f = _GadgetT %~ f
{-# INLINABLE belowGadgetT #-}

underGadgetT
    :: (ReaderT a (StateT s m) c -> ReaderT a' (StateT s' m') c')
    -> GadgetT a s m c
    -> GadgetT a' s' m' c'
underGadgetT f = _Wrapping GadgetT %~ f
{-# INLINABLE underGadgetT #-}

overGadgetT
    :: (GadgetT a s m c -> GadgetT a' s' m' c')
    -> ReaderT a (StateT s m) c
    -> ReaderT a' (StateT s' m') c'
overGadgetT f = _Unwrapping GadgetT %~ f
{-# INLINABLE overGadgetT #-}

aboveGadgetT ::
  (GadgetT a s m c -> GadgetT a' s' m' c')
  -> (a -> s -> m (c, s)) -> a' -> s' -> m' (c', s')
aboveGadgetT f = from _GadgetT %~ f
{-# INLINABLE aboveGadgetT #-}

instance MonadTrans (GadgetT a s) where
    lift = GadgetT . lift . lift

instance MFunctor (GadgetT a s) where
    hoist f (GadgetT m) = GadgetT (hoist (hoist f) m)

instance (Monad m, Semigroup c) => Semigroup (GadgetT a s m c) where
    (GadgetT f) <> (GadgetT g) = GadgetT $ (<>) <$> f <*> g
    {-# INLINABLE (<>) #-}

instance (Monad m, Monoid c) => Monoid (GadgetT a s m c) where
    mempty = GadgetT $ pure mempty
    {-# INLINABLE mempty #-}

    (GadgetT f) `mappend` (GadgetT g) = GadgetT $ mappend <$> f <*> g
    {-# INLINABLE mappend #-}

-- | zoom can be used to modify the state inside an Gadget
type instance Zoomed (GadgetT a s m) = Zoomed (ReaderT a (StateT s m))
instance Monad m => Zoom (GadgetT a s m) (GadgetT a t m) s t where
    zoom l = GadgetT . zoom l . runGadgetT
    {-# INLINABLE zoom #-}

-- | magnify can be used to modify the action inside an Gadget
type instance Magnified (GadgetT a s m) = Magnified (ReaderT a (StateT s m))
instance Monad m => Magnify (GadgetT a s m) (GadgetT b s m) a b where
    magnify l = GadgetT . magnify l . runGadgetT
    {-# INLINABLE magnify #-}

type instance Implanted (GadgetT a s m c) = Zoomed (GadgetT a s m) c
instance Monad m => Implant (GadgetT a s m c) (GadgetT a t m c) s t where
    implant = zoom
    {-# INLINABLE implant #-}

type instance Dispatched (GadgetT a s m c) = Magnified (GadgetT a s m) c
instance Monad m => Dispatch (GadgetT a s m c) (GadgetT b s m c) a b where
    dispatch = magnify
    {-# INLINABLE dispatch #-}
