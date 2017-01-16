{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Functional version of (Elm Brief/View & startApp architecture) enabling composable widgets, and a FRP-like framework.
--
-- This framework makes it easier to modularize the Elm architecture idea of View/Brief:
-- based on the deprecated Elm Architecture version of Jan 2016
-- https://github.com/evancz/elm-architecture-tutorial/tree/de5682a5a8e4459aed4637533adb25e462f8a2ae
--
-- The Elm View/Brief is basically as follows:
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
-- This module uses isomorphic implementations Brief and View resulting in instances can be be composed together into larger Widgets.
-- Original inspiration from https://arianvp.me/lenses-and-prisms-for-modular-clientside-apps/
--
-- This framework provides three main combinators:
-- * Semigroup and Monoid instances for concatenating widgets.
-- * 'dispatch' is used to re-route the action type.
-- * 'implant' is used to modify the model type.
module Glazier
    ( Window(..)
    , Implanted
    , Implant(..)
    , Dispatched
    , Dispatch(..)
    ) where

import Control.Applicative
import Control.Lens
import qualified Control.Lens.Internal.Zoom as Z
import Control.Monad.Reader
import Data.Semigroup
import Prelude hiding (id, (.))

-------------------------------------------------------------------------------

-- | The Elm view function is basically @view :: model -> html@
-- NB. elm-html is actually @view :: Signal.Address action -> model -> html@
-- where @Signal.Address action@ is the Pipes.Concurrent.Output that is sent
-- actions (eg. when html button is clicked).
-- This address argument is not required in the general case, and is only required for specific widgets on an as needed basis.
-- Therefore, using the fundamental type of @view :: model -> html@
-- This is be ehanced with monadic effects with ReaderT.
-- This is named Window instead of View to avoid confusion with view from Control.Lens
newtype Window s m v = Window { runWindow :: ReaderT s m v }
  deriving (MonadReader s, Monad, Applicative, Functor)

makeWrapped ''Window

instance (Applicative m, Semigroup v) => Semigroup (Window s m v) where
    (Window f) <> (Window g) = Window $ ReaderT $ \a ->
        (<>) <$> runReaderT f a <*> runReaderT g a

instance (Applicative m, Monoid v) => Monoid (Window s m v) where
    mempty = Window $ ReaderT $ const $ pure mempty
    (Window f) `mappend` (Window g) = Window $ ReaderT $ \a ->
        mappend <$> runReaderT f a <*> runReaderT g a

-------------------------------------------------------------------------------

-- | Modify the state given a lens, prism or traversal.
-- NB. This is 'Control.Lens.Zoom' for Notify.
type family Implanted m :: * -> *
class Implant m n s t | m -> s, n -> t, m t -> n, n s -> m where
  implant :: LensLike' (Implanted m) t s -> m -> n

type instance Implanted (Window s m v) = Z.Effect m v
instance Monad m => Implant (Window s m v) (Window t m v) s t where
  implant l (Window m) = Window $ magnify l m

-------------------------------------------------------------------------------
type family Dispatched m :: * -> *

-- | Changes the action type given a lens, prism or traversal
class Dispatch m n b a | m -> b, n -> a, m a -> n, n b -> m where
  dispatch :: LensLike' (Dispatched m) a b -> m -> n
