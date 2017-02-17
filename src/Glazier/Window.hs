{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Functional version of (Elm View/Update & startApp architecture) enabling composable widgets, and a FRP-like framework.
--
-- This framework makes it easier to modularize the Elm architecture idea of View/Update:
-- based on the deprecated Elm Architecture version of Jan 2016
-- https://github.com/evancz/elm-architecture-tutorial/tree/de5682a5a8e4459aed4637533adb25e462f8a2ae
--
-- The Elm View/Update is basically as follows:
--
-- @
-- data Model = Blah....
-- data Action = DoThis | DoThat deriving Show
--
-- -- | update is fired from an event processing loop
-- update :: Action -> Model -> Model
--
-- -- | The widget from 'view' knows how to send Action to a mailbox
-- view :: Signal Address -> Model -> Html
-- @
--
-- This module uses isomorphic Window and Gadget resulting in instances can be be composed together into larger Widgets.
-- Original inspiration from https://arianvp.me/lenses-and-prisms-for-modular-clientside-apps/
--
-- This framework provides three main combinators:
-- * Semigroup and Monoid instances for concatenating widgets.
-- * 'dispatch' is used to re-route the action type.
-- * 'implant' is used to modify the model type.
module Glazier.Window where

import Control.Applicative
import Control.Lens
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix (MonadFix)
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Semigroup
import Glazier.Class

-------------------------------------------------------------------------------

-- | The Elm view function is basically @view :: model -> html@
-- This is be ehanced with monadic effects with ReaderT.
-- The render output can be wrapped in a WriterT to make it more composable.
-- We use a CPS-style WriterT (ie a StateT) to avoid space leaks.
-- This is named Window instead of View to avoid confusion with view from Control.Lens
-- NB. This is the same formulation as 'Glaizer.GadgetT'.
-- The only difference is 'WindowT' only has 'Glazier.Implant' instance.
newtype WindowT s v m r = WindowT
    { runWindowT :: ReaderT s (StateT v m) r
    } deriving ( MonadState v
               , MonadReader s
               , Monad
               , Applicative
               , Functor
               , Fail.MonadFail
               , Alternative
               , MonadPlus
               , MonadFix
               , MonadIO
               )

makeWrapped ''WindowT

type Window s v = WindowT s v Identity

_WindowT :: Iso (WindowT s v m r) (WindowT s' v' m' r') (s -> v -> m (r, v)) (s' -> v' -> m' (r', v'))
_WindowT = _Wrapping WindowT . iso runReaderT ReaderT . iso (runStateT .) (StateT .)
{-# INLINABLE _WindowT #-}

-- | Non polymorphic version of _Window
_WindowT' :: Iso' (WindowT s v m r) (s -> v -> m (r, v))
_WindowT' = _WindowT
{-# INLINABLE _WindowT' #-}

mkWindowT' :: (s -> v -> m (r, v)) -> WindowT s v m r
mkWindowT' = review _WindowT
{-# INLINABLE mkWindowT' #-}

runWindowT' :: WindowT s v m r -> (s -> v -> m (r, v))
runWindowT' = view _WindowT
{-# INLINABLE runWindowT' #-}

belowWindowT ::
  ((s -> v -> m (r, v)) -> s' -> v' -> m' (r', v'))
  -> WindowT s v m r -> WindowT s' v' m' r'
belowWindowT f = _WindowT %~ f
{-# INLINABLE belowWindowT #-}

underWindowT
    :: (ReaderT s (StateT v m) r -> ReaderT s' (StateT v' m') r')
    -> WindowT s v m r
    -> WindowT s' v' m' r'
underWindowT f = _Wrapping WindowT %~ f
{-# INLINABLE underWindowT #-}

overWindowT
    :: (WindowT s v m r -> WindowT s' v' m' r')
    -> ReaderT s (StateT v m) r
    -> ReaderT s' (StateT v' m') r'
overWindowT f = _Unwrapping WindowT %~ f
{-# INLINABLE overWindowT #-}

aboveWindowT ::
  (WindowT s v m r -> WindowT s' v' m' r')
  -> (s -> v -> m (r, v)) -> s' -> v' -> m' (r', v')
aboveWindowT f = from _WindowT %~ f
{-# INLINABLE aboveWindowT #-}

instance MonadTrans (WindowT s v) where
    lift = WindowT . lift . lift

instance MFunctor (WindowT s v) where
    hoist f (WindowT m) = WindowT (hoist (hoist f) m)

instance (Monad m, Semigroup r) => Semigroup (WindowT s v m r) where
    (WindowT f) <> (WindowT g) = WindowT $ (<>) <$> f <*> g
    {-# INLINABLE (<>) #-}

instance (Monad m, Monoid r) => Monoid (WindowT s v m r) where
    mempty = WindowT $ pure mempty
    {-# INLINABLE mempty #-}

    (WindowT f) `mappend` (WindowT g) = WindowT $ mappend <$> f <*> g
    {-# INLINABLE mappend #-}

-- | zoom can be used to modify the state inside an Gadget
type instance Zoomed (WindowT s v m) = Zoomed (ReaderT s (StateT v m))
instance Monad m => Zoom (WindowT s v m) (WindowT s u m) v u where
    zoom l = WindowT . zoom l . runWindowT
    {-# INLINABLE zoom #-}

-- | magnify can be used to modify the action inside an Gadget
type instance Magnified (WindowT s v m) = Magnified (ReaderT s (StateT v m))
instance Monad m => Magnify (WindowT s v m) (WindowT t v m) s t where
    magnify l = WindowT . magnify l . runWindowT
    {-# INLINABLE magnify #-}

type instance Implanted (WindowT s v m r) = Magnified (WindowT s v m) r
instance Monad m => Implant (WindowT s v m r) (WindowT t v m r) s t where
    implant = magnify
    {-# INLINABLE implant #-}
