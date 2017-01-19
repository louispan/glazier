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
    , liftWindow
    , hoistWindow
    , underWindow
    , Implanted
    , Implant(..)
    , Dispatched
    , Dispatch(..)
    ) where

import Control.Applicative
import Control.Arrow
import qualified Control.Category as C
import Control.Lens
import qualified Control.Lens.Internal.Zoom as Z
import qualified Control.Monad.Fail as Fail
import Control.Monad.Reader
import Data.Semigroup
import Control.Monad.Fix (MonadFix)
import Control.Monad.Zip (MonadZip)
import Control.Monad.Morph
import Data.Profunctor

-------------------------------------------------------------------------------

-- | The Elm view function is basically @view :: model -> html@
-- NB. elm-html is actually @view :: Signal.Address action -> model -> html@
-- where @Signal.Address action@ is the Pipes.Concurrent.Output that is sent
-- actions (eg. when html button is clicked).
-- This address argument is not required in the general case, and is only required for specific widgets on an as needed basis.
-- Therefore, using the fundamental type of @view :: model -> html@
-- This is be ehanced with monadic effects with ReaderT.
-- This is named Window instead of View to avoid confusion with view from Control.Lens
newtype Window m s v = Window
    { runWindow :: ReaderT s m v
    } deriving ( MonadReader s
               , Monad
               , Applicative
               , Functor
               , Fail.MonadFail
               , Alternative
               , MonadPlus
               , MonadFix
               , MonadIO
               , MonadZip
               )

makeWrapped ''Window

liftWindow :: (MonadTrans t, Monad m) => Window m s v -> Window (t m) s v
liftWindow (Window (ReaderT f)) = Window $ ReaderT $ \a -> lift (f a)

hoistWindow :: (Monad m) => (forall a. m a -> n a) -> Window m s v -> Window n s v
hoistWindow f (Window m) = Window $ hoist f m

underWindow :: ((s -> m v) -> (s' -> m' v')) -> Window m s v -> Window m' s' v'
underWindow f (Window (ReaderT m)) = Window . ReaderT $ f m

instance (Applicative m, Semigroup v) => Semigroup (Window m s v) where
    (Window f) <> (Window g) = Window $ ReaderT $ \a ->
        (<>) <$> runReaderT f a <*> runReaderT g a

instance (Applicative m, Monoid v) => Monoid (Window m s v) where
    mempty = Window $ ReaderT $ const $ pure mempty
    (Window f) `mappend` (Window g) = Window $ ReaderT $ \a ->
        mappend <$> runReaderT f a <*> runReaderT g a

instance Monad m => Profunctor (Window m) where
    dimap f g = underWindow (runKleisli . dimap f g . Kleisli)

instance Monad m => Strong (Window m) where
    first' = underWindow (runKleisli . first' . Kleisli)

instance Monad m => C.Category (Window m) where
    id = Window . ReaderT $ runKleisli C.id
    Window (ReaderT k) . Window (ReaderT l) = Window . ReaderT . runKleisli $ Kleisli k C.. Kleisli l

instance Monad m => Arrow (Window m) where
    arr f = Window $ ReaderT $ runKleisli $ arr f
    first = underWindow (runKleisli . first . Kleisli)

instance Monad m => Choice (Window m) where
    left' = underWindow (runKleisli . left' . Kleisli)

instance Monad m => ArrowChoice (Window m) where
    left = underWindow (runKleisli . left . Kleisli)

instance Monad m => ArrowApply (Window m) where
    app = Window . ReaderT $ \(Window (ReaderT bc), b) -> bc b

instance MonadPlus m => ArrowZero (Window m) where
    zeroArrow = Window mzero

instance MonadPlus m => ArrowPlus (Window m) where
    Window a <+> Window b = Window (a `mplus` b)

-------------------------------------------------------------------------------

-- | Modify the state given a lens, prism or traversal.
-- NB. This is 'Control.Lens.Zoom' for Notify.
type family Implanted m :: * -> *
class Implant m n s t | m -> s, n -> t, m t -> n, n s -> m where
  implant :: LensLike' (Implanted m) t s -> m -> n

type instance Implanted (Window m s v) = Z.Effect m v
instance Monad m => Implant (Window m s v) (Window m t v) s t where
  implant l (Window m) = Window $ magnify l m

-------------------------------------------------------------------------------
type family Dispatched m :: * -> *

-- | Changes the action type given a lens, prism or traversal
class Dispatch m n b a | m -> b, n -> a, m a -> n, n b -> m where
  dispatch :: LensLike' (Dispatched m) a b -> m -> n
