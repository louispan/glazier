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
import Control.Monad.Except
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix (MonadFix)
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Semigroup

-- | The Elm update function is @a -> s -> (s, c)@
-- This is isomorphic to @ReaderT a (State s) c@
-- ie, given an action "a", and a current state "s", return the new state "s"
-- and any commands "c" that need to be interpreted externally (eg. download file).
-- This is named Gadget instead of Update to avoid confusion with update from Data.Map
-- It is also enhanced with ExceptT to get Alternative instance and termination with error code.
newtype GadgetT a e s m c = GadgetT
    { runGadgetT :: ReaderT a (ExceptT e (StateT s m)) c
    } deriving ( MonadError e
               , MonadReader a
               , MonadState s
               , Functor
               , Applicative
               , Monad
               , Fail.MonadFail
               , Alternative
               , MonadPlus
               , MonadFix
               , MonadIO
               )

makeWrapped ''GadgetT

type Gadget a e s = GadgetT a e s Identity

_GadgetT :: Iso (GadgetT a e s m c) (GadgetT a' e' s' m' c') (a -> s -> m (Either e c, s)) (a' -> s' -> m' (Either e' c', s'))
_GadgetT = _Wrapping GadgetT . iso runReaderT ReaderT . iso (runExceptT .) (ExceptT .) . iso (runStateT .) (StateT .)
{-# INLINABLE _GadgetT #-}

-- | Non polymorphic version of _Gadget
_GadgetT' :: Iso' (GadgetT a e s m c) (a -> s -> m (Either e c, s))
_GadgetT' = _GadgetT
{-# INLINABLE _GadgetT' #-}

mkGadgetT' :: (a -> s -> m (Either e c, s)) -> GadgetT a e s m c
mkGadgetT' = review _GadgetT
{-# INLINABLE mkGadgetT' #-}

runGadgetT' :: GadgetT a e s m c -> (a -> s -> m (Either e c, s))
runGadgetT' = view _GadgetT
{-# INLINABLE runGadgetT' #-}

belowGadgetT ::
  ((a -> s -> m (Either e c, s)) -> a' -> s' -> m' (Either e' c', s'))
  -> GadgetT a e s m c -> GadgetT a' e' s' m' c'
belowGadgetT f = _GadgetT %~ f
{-# INLINABLE belowGadgetT #-}

underGadgetT
    :: (ReaderT a (ExceptT e (StateT s m)) c -> ReaderT a' (ExceptT e' (StateT s' m')) c')
    -> GadgetT a e s m c
    -> GadgetT a' e' s' m' c'
underGadgetT f = _Wrapping GadgetT %~ f
{-# INLINABLE underGadgetT #-}

overGadgetT
    :: (GadgetT a e s m c -> GadgetT a' e' s' m' c')
    -> ReaderT a (ExceptT e (StateT s m)) c
    -> ReaderT a' (ExceptT e' (StateT s' m')) c'
overGadgetT f = _Unwrapping GadgetT %~ f
{-# INLINABLE overGadgetT #-}

aboveGadgetT ::
  (GadgetT a e s m c -> GadgetT a' e' s' m' c')
  -> (a -> s -> m (Either e c, s)) -> a' -> s' -> m' (Either e' c', s')
aboveGadgetT f = from _GadgetT %~ f
{-# INLINABLE aboveGadgetT #-}

withExceptT' :: Functor m => (e -> e') -> GadgetT a e s m c -> GadgetT a e' s m c
withExceptT' f = (_Wrapping GadgetT . iso runReaderT ReaderT) %~ (withExceptT f .)

instance MonadTrans (GadgetT a e s) where
    lift = GadgetT . lift . lift . lift

instance MFunctor (GadgetT a e s) where
    hoist f (GadgetT m) = GadgetT (hoist (hoist (hoist f)) m)

instance (Monad m, Semigroup c) => Semigroup (GadgetT a e s m c) where
    (<>) = liftA2 (<>)
    {-# INLINABLE (<>) #-}

instance (Monad m, Monoid c) => Monoid (GadgetT a e s m c) where
    mempty = pure mempty
    {-# INLINABLE mempty #-}

    mappend = liftA2 mappend
    {-# INLINABLE mappend #-}

-- | zoom can be used to modify the state inside an Gadget
-- This requires UndecidableInstances but is safe because (ReaderT a (ExceptT e (StateT s m)))
-- is smaller than (GadgetT a s m)
type instance Zoomed (GadgetT a e s m) = Zoomed (ReaderT a (ExceptT e (StateT s m)))
instance Monad m => Zoom (GadgetT a e s m) (GadgetT a e t m) s t where
    zoom l = GadgetT . zoom l . runGadgetT
    {-# INLINABLE zoom #-}

-- | magnify can be used to modify the action inside an Gadget
-- This requires UndecidableInstances but is safe because (ReaderT a (ExceptT e (StateT s m)))
type instance Magnified (GadgetT a e s m) = Magnified (ReaderT a (ExceptT e (StateT s m)))
instance Monad m => Magnify (GadgetT b e s m) (GadgetT a e s m) b a where
    magnify l = GadgetT . magnify l . runGadgetT
    {-# INLINABLE magnify #-}
