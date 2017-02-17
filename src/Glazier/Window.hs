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
import Data.Semigroup
import Glazier.Class

-------------------------------------------------------------------------------

-- | The Elm view function is basically @view :: model -> html@
-- This can be enhanced with monadic effects with ReaderT.
-- This is named Window instead of View to avoid confusion with view from Control.Lens
newtype WindowT s m v = WindowT
    { runWindowT :: ReaderT s m v
    } deriving ( MonadReader s
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

type Window s = WindowT s Identity

_WindowT :: Iso (WindowT s m v) (WindowT s' m' v') (s -> m v) (s' -> m' v')
_WindowT = _Wrapping WindowT . iso runReaderT ReaderT
{-# INLINABLE _WindowT #-}

-- | Non polymorphic version of _Window
_WindowT' :: Iso' (WindowT s m v) (s -> m v)
_WindowT' = _WindowT
{-# INLINABLE _WindowT' #-}

mkWindowT' :: (s -> m v) -> WindowT s m v
mkWindowT' = review _WindowT
{-# INLINABLE mkWindowT' #-}

runWindowT' :: WindowT s m v -> (s -> m v)
runWindowT' = view _WindowT
{-# INLINABLE runWindowT' #-}

belowWindowT ::
  ((s -> m v) -> (s' -> m' v'))
  -> WindowT s m v -> WindowT s' m' v'
belowWindowT f = _WindowT %~ f
{-# INLINABLE belowWindowT #-}

underWindowT
    :: (ReaderT s m v -> ReaderT s' m' v')
    -> WindowT s m v
    -> WindowT s' m' v'
underWindowT f = _Wrapping WindowT %~ f
{-# INLINABLE underWindowT #-}

overWindowT
    :: (WindowT s m v -> WindowT s' m' v')
    -> ReaderT s m v
    -> ReaderT s' m' v'
overWindowT f = _Unwrapping WindowT %~ f
{-# INLINABLE overWindowT #-}

aboveWindowT ::
  (WindowT s m v -> WindowT s' m' v')
  -> (s -> m v) -> (s' -> m' v')
aboveWindowT f = from _WindowT %~ f
{-# INLINABLE aboveWindowT #-}

instance MonadTrans (WindowT s) where
    lift = WindowT . lift

instance MFunctor (WindowT s) where
    hoist f (WindowT m) = WindowT (hoist f m)

instance (Applicative m, Semigroup v) => Semigroup (WindowT s m v) where
    (WindowT f) <> (WindowT g) = WindowT $ (<>) <$> f <*> g
    {-# INLINABLE (<>) #-}

instance (Applicative m, Monoid v) => Monoid (WindowT s m v) where
    mempty = WindowT $ pure mempty
    {-# INLINABLE mempty #-}

    (WindowT f) `mappend` (WindowT g) = WindowT $ mappend <$> f <*> g
    {-# INLINABLE mappend #-}

-- | magnify can be used to modify the action inside an Gadget
type instance Magnified (WindowT s m) = Magnified (ReaderT s m)
instance Monad m => Magnify (WindowT s m) (WindowT t m) s t where
    magnify l = WindowT . magnify l . runWindowT
    {-# INLINABLE magnify #-}

type instance Implanted (WindowT s m v) = Magnified (WindowT s m) v
instance Monad m => Implant (WindowT s m v) (WindowT t m v) s t where
    implant = magnify
    {-# INLINABLE implant #-}
