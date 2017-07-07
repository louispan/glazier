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
import Control.Monad.Trans.Maybe
import Data.Profunctor.Unsafe
import Data.Semigroup
import Glazier.Internal

-- | The Elm update function is @a -> s -> (s, c)@
-- This is isomorphic to @ReaderT a (State s) c@
-- ie, given an action "a", and a current state "s", return the new state "s"
-- and any commands "c" that need to be interpreted externally (eg. download file).
-- This is named Gadget instead of Update to avoid confusion with update from Data.Map
-- This is also enhanced with MaybeT for its Alternative instance.
newtype GadgetT a s m c = GadgetT
    { runGadgetT :: ReaderT a (MaybeT (StateT s m)) c
    } deriving ( MonadReader a
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

type Gadget a s = GadgetT a s Identity

_GadgetT :: Iso (GadgetT a s m c) (GadgetT a' s' m' c') (a -> s -> m (Maybe c, s)) (a' -> s' -> m' (Maybe c', s'))
_GadgetT = _Wrapping GadgetT . iso runReaderT ReaderT . iso (runMaybeT .) (MaybeT .) . iso (runStateT .) (StateT .)

-- | Non polymorphic version of _Gadget
_GadgetT' :: Iso' (GadgetT a s m c) (a -> s -> m (Maybe c, s))
_GadgetT' = _GadgetT

mkGadgetT' :: (a -> s -> m (Maybe c, s)) -> GadgetT a s m c
mkGadgetT' = review _GadgetT

runGadgetT' :: GadgetT a s m c -> (a -> s -> m (Maybe c, s))
runGadgetT' = view _GadgetT

belowGadgetT ::
  ((a -> s -> m (Maybe c, s)) -> a' -> s' -> m' (Maybe c', s'))
  -> GadgetT a s m c -> GadgetT a' s' m' c'
belowGadgetT f = _GadgetT %~ f

underGadgetT
    :: (ReaderT a (MaybeT (StateT s m)) c -> ReaderT a' (MaybeT (StateT s' m')) c')
    -> GadgetT a s m c
    -> GadgetT a' s' m' c'
underGadgetT f = _Wrapping GadgetT %~ f

overGadgetT
    :: (GadgetT a s m c -> GadgetT a' s' m' c')
    -> ReaderT a (MaybeT (StateT s m)) c
    -> ReaderT a' (MaybeT (StateT s' m')) c'
overGadgetT f = _Unwrapping GadgetT %~ f

aboveGadgetT ::
  (GadgetT a s m c -> GadgetT a' s' m' c')
  -> (a -> s -> m (Maybe c, s)) -> a' -> s' -> m' (Maybe c', s')
aboveGadgetT f = from _GadgetT %~ f

-- | Runs a GadgetT with a given action
withGadgetT :: a -> GadgetT a s m c -> GadgetT a' s m c
withGadgetT a (GadgetT (ReaderT k)) = GadgetT . ReaderT . const $ k a

instance MonadTrans (GadgetT a s) where
    lift = GadgetT . lift . lift . lift

instance MFunctor (GadgetT a s) where
    hoist f (GadgetT m) = GadgetT (hoist (hoist (hoist f)) m)

instance (Monad m, Semigroup c) => Semigroup (GadgetT a s m c) where
    (<>) = liftA2 (<>)

instance (Monad m, Monoid c) => Monoid (GadgetT a s m c) where
    mempty = pure mempty

    mappend = liftA2 mappend

-- | zoom can be used to modify the state inside an Gadget
-- This requires UndecidableInstances but is safe because (ReaderT a (MaybeT (StateT s m)))
-- is smaller than (GadgetT a s m)
type instance Zoomed (GadgetT a s m) = Zoomed (ReaderT a (MaybeT (StateT s m)))
instance Monad m => Zoom (GadgetT a s m) (GadgetT a t m) s t where
    zoom l = GadgetT . zoom l . runGadgetT

-- | magnify can be used to modify the action inside an Gadget
-- This requires UndecidableInstances but is safe because (StateT s m)
-- is smaller than (GadgetT a s m)
-- This instance means magnifying with a Prism will result in 'empty' if the traversal fails.
-- Otherwise it will result in mappened results of the monoid inside the Maybe.
type instance Magnified (GadgetT a s m) = EffectMay (StateT s m)
instance Monad m => Magnify (GadgetT b s m) (GadgetT a s m) b a where
    magnify l (GadgetT (ReaderT m)) =
        GadgetT . ReaderT . fmap MaybeT $ getEffectMay #. l (EffectMay #. (runMaybeT <$> m))
