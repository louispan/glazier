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
import Control.Lens.Internal.Zoom
import Control.Monad.Except
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix (MonadFix)
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Profunctor.Unsafe
import Data.Semigroup
import Glazier.Internal

-- | The Elm update function is @a -> s -> (s, c)@
-- This is isomorphic to @ReaderT a (State s) c@
-- ie, given an action "a", and a current state "s", return the new state "s"
-- and any commands "c" that need to be interpreted externally (eg. download file).
-- This is named Gadget instead of Update to avoid confusion with update from Data.Map
-- It is also enhanced with ExceptT to get Alternative instance and termination with error code.
newtype GadgetT e a s m c = GadgetT
    { runGadgetT :: ExceptT e (ReaderT a (StateT s m)) c
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

type Gadget e a s = GadgetT e a s Identity

_GadgetT :: Iso (GadgetT e a s m c) (GadgetT e' a' s' m' c') (a -> s -> m (Either e c, s)) (a' -> s' -> m' (Either e' c', s'))
_GadgetT = _Wrapping GadgetT . iso runExceptT ExceptT . iso runReaderT ReaderT . iso (runStateT .) (StateT .)
{-# INLINABLE _GadgetT #-}

-- | Non polymorphic version of _Gadget
_GadgetT' :: Iso' (GadgetT e a s m c) (a -> s -> m (Either e c, s))
_GadgetT' = _GadgetT
{-# INLINABLE _GadgetT' #-}

mkGadgetT' :: (a -> s -> m (Either e c, s)) -> GadgetT e a s m c
mkGadgetT' = review _GadgetT
{-# INLINABLE mkGadgetT' #-}

runGadgetT' :: GadgetT e a s m c -> (a -> s -> m (Either e c, s))
runGadgetT' = view _GadgetT
{-# INLINABLE runGadgetT' #-}

belowGadgetT ::
  ((a -> s -> m (Either e c, s)) -> a' -> s' -> m' (Either e' c', s'))
  -> GadgetT e a s m c -> GadgetT e' a' s' m' c'
belowGadgetT f = _GadgetT %~ f
{-# INLINABLE belowGadgetT #-}

underGadgetT
    :: (ExceptT e (ReaderT a (StateT s m)) c -> ExceptT e' (ReaderT a' (StateT s' m')) c')
    -> GadgetT e a s m c
    -> GadgetT e' a' s' m' c'
underGadgetT f = _Wrapping GadgetT %~ f
{-# INLINABLE underGadgetT #-}

overGadgetT
    :: (GadgetT e a s m c -> GadgetT e' a' s' m' c')
    -> ExceptT e (ReaderT a (StateT s m)) c
    -> ExceptT e' (ReaderT a' (StateT s' m')) c'
overGadgetT f = _Unwrapping GadgetT %~ f
{-# INLINABLE overGadgetT #-}

aboveGadgetT ::
  (GadgetT e a s m c -> GadgetT e' a' s' m' c')
  -> (a -> s -> m (Either e c, s)) -> a' -> s' -> m' (Either e' c', s')
aboveGadgetT f = from _GadgetT %~ f
{-# INLINABLE aboveGadgetT #-}

withExceptT' :: Functor m => (e -> e') -> GadgetT e a s m c -> GadgetT e' a s m c
withExceptT' f = underGadgetT (withExceptT f)

instance MonadTrans (GadgetT e a s) where
    lift = GadgetT . lift . lift . lift

instance MFunctor (GadgetT e a s) where
    hoist f (GadgetT m) = GadgetT (hoist (hoist (hoist f)) m)

instance (Monad m, Semigroup c) => Semigroup (GadgetT e a s m c) where
    (<>) = liftA2 (<>)
    {-# INLINABLE (<>) #-}

instance (Monad m, Monoid c) => Monoid (GadgetT e a s m c) where
    mempty = pure mempty
    {-# INLINABLE mempty #-}

    mappend = liftA2 mappend
    {-# INLINABLE mappend #-}

-- | zoom can be used to modify the state inside an Gadget
-- This requires UndecidableInstances but is safe because (ExceptT e (ReaderT a (StateT s m)))
-- is smaller than (GadgetT a s m)
type instance Zoomed (GadgetT e a s m) = Zoomed (ExceptT e (ReaderT a (StateT s m)))
instance Monad m => Zoom (GadgetT e a s m) (GadgetT e a t m) s t where
    zoom l = GadgetT . zoom l . runGadgetT
    {-# INLINABLE zoom #-}

-- | magnify can be used to modify the action inside an Gadget
-- This requires UndecidableInstances but is safe because (EffectErr e (StateT s m))
-- is smaller than (GadgetT a s m)
type instance Magnified (GadgetT e a s m) = EffectErr e (StateT s m)
instance Monad m => Magnify (GadgetT e b s m) (GadgetT e a s m) b a where
    magnify l (GadgetT (ExceptT m)) = GadgetT . ExceptT . fmap getErr . ReaderT $ getEffectErr #. l (EffectErr #. runReaderT (Err <$> m))
    {-# INLINABLE magnify #-}
