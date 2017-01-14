{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
module Glazier.Core
    ( Depict(..)
    , Notify(..)
    , Widget(..)
    , HasDepict(..)
    , HasNotify(..)
    , statically
    , dynamically
    , Implanted
    , Implant(..)
    , Dispatch(..)
    ) where

import Control.Applicative
import Control.Category
import Control.Lens
import Control.Lens.Internal.Zoom
import Control.Monad.Reader
import Control.Monad.RWS.Strict hiding ((<>))
import Data.Biapplicative
import Data.Functor.Apply
import Data.Maybe
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
-- This is named Depict instead of View to avoid confusion with view from Control.Lens
newtype Depict s m d = Depict { getDepict :: ReaderT s m d }
  deriving (MonadReader s, Monad, Applicative, Functor)

makeWrapped ''Depict

instance (Applicative m, Semigroup c) => Semigroup (Depict s m c) where
    (Depict f) <> (Depict g) = Depict $ ReaderT $ \a ->
        (<>) <$> runReaderT f a <*> runReaderT g a

instance (Applicative m, Monoid c) => Monoid (Depict s m c) where
    mempty = Depict $ ReaderT $ const $ pure mempty
    (Depict f) `mappend` (Depict g) = Depict $ ReaderT $ \a ->
        mappend <$> runReaderT f a <*> runReaderT g a

-------------------------------------------------------------------------------

-- | The Elm update function is @a -> s -> (s, c)@
-- This is isomorphic to @ReaderT a (State s) c@
-- ie, given an action "a", and a current state "s", return the new state "s"
-- and any commands "c" that need to be interpreted externally (eg. download file).
-- This is named Notify instead of Update to avoid confusion with update from Data.Map
-- This is also further enhanced with monadic and Writer effect, so we can just use RWST to avoid
-- writing new code.
newtype Notify a w s m c = Notify { getNotify :: RWST a w s m c }
  deriving (MonadState s, MonadReader a, Monad, Applicative, Functor)

makeWrapped ''Notify

instance (Monad m, Monoid w, Semigroup c) => Semigroup (Notify a w s m c) where
   (Notify f) <> (Notify g) = Notify $ (<>) <$> f <*> g

instance (Monad m, Monoid w, Monoid c) => Monoid (Notify a w s m c) where
  mempty = Notify $ RWST $ \_ s -> pure (mempty, s, mempty)
  (Notify f) `mappend` (Notify g) = Notify $ mappend <$> f <*> g

-------------------------------------------------------------------------------

-- | A widget is basically a tuple with Notify and Depict.
data Widget a w s m c d = Widget
  { widgetNotify :: Notify a w s m c -- a -> s -> (s, c)
  , widgetDepict :: Depict s m d -- s -> d
  }

makeFields ''Widget

instance (Monad m, Monoid w, Semigroup c, Semigroup d) => Semigroup (Widget a w s m c d) where
  w1 <> w2 = Widget
    (widgetNotify w1 <> widgetNotify w2)
    (widgetDepict w1 <> widgetDepict w2)

instance (Monad m, Monoid w, Monoid c, Monoid d) => Monoid (Widget a w s m c d) where
  mempty = Widget mempty mempty
  mappend w1 w2 = Widget
    (widgetNotify w1 `mappend` widgetNotify w2)
    (widgetDepict w1 `mappend` widgetDepict w2)

instance Functor m => Bifunctor (Widget a w s m) where
  bimap f g w = Widget
    (f <$> widgetNotify w)
    (g <$> widgetDepict w)

instance (Monad m, Monoid w) => Biapplicative (Widget a w s m) where
  bipure a b = Widget
    (pure a)
    (pure b)
  f <<*>> a = Widget
    (widgetNotify f <*> widgetNotify a)
    (widgetDepict f <*> widgetDepict a)

statically :: (Monad m, Monoid w, Monoid c) => Depict s m d -> Widget a w s m c d
statically = Widget mempty

dynamically :: (Monad m, Monoid d) => Notify a w s m c -> Widget a w s m c d
dynamically v = Widget v mempty

-- -------------------------------------------------------------------------------

-- | magnify can be used to modify the action inside an Notify
type instance Magnified (Notify a w s m) = Magnified (RWST a w s m)
instance (Monad m, Monoid w) => Magnify (Notify a w s m) (Notify b w s m) a b where
  magnify l = Notify . magnify l . getNotify
  {-# INLINE magnify #-}

-- | zoom can be used to modify the state inside an Notify
type instance Zoomed (Notify a w s m) = Zoomed (RWST a w s m)
instance (Monad m, Monoid w) => Zoom (Notify a w s m) (Notify a w t m) s t where
  zoom l = Notify . zoom l . getNotify
  {-# INLINE zoom #-}

-------------------------------------------------------------------------------

-- | Modify the state given a lens, prism or traversal.
-- NB. This is 'Control.Lens.Zoom' for Notify.
type family Implanted m :: * -> *
class Implant m n s t | m -> s, n -> t, m t -> n, n s -> m where
  implant :: LensLike' (Implanted m) t s -> m -> n

type instance Implanted (Depict s m d) = Effect m d
instance Monad m => Implant (Depict s m d) (Depict t m d) s t where
  implant l = Depict . magnify l . getDepict

type instance Implanted (Notify a w s m c) = Zoomed (Notify a w s m) c
instance (Monad m, Monoid w) => Implant (Notify a w s m c) (Notify a w t m c) s t where
  implant = zoom

type instance Implanted (Widget a w s m c d) = PairMaybeFunctor (Implanted (Notify a w s m c)) (Implanted (Depict s m d))
instance (Monad m, Monoid w) => Implant (Widget a w s m c d) (Widget a w t m c d) s t where
  implant l w = Widget
    (implant (fstLensLike l) $ widgetNotify w)
    (implant (sndLensLike l) $ widgetDepict w)

-------------------------------------------------------------------------------

-- | Changes the action type given a prism
class Dispatch m n a b | m -> a, n -> b, m b -> n, n a -> m where
  dispatch :: Prism' b a -> m -> n

instance (Monad m, Monoid w, Monoid c) => Dispatch (Notify a w s m c) (Notify b w s m c) a b where
  dispatch = magnify

instance (Monad m, Monoid w, Monoid c) => Dispatch (Widget a w s m c v) (Widget b w s m c v) a b where
  dispatch p w = Widget
    (dispatch p $ widgetNotify w)
    (widgetDepict w)

-- -------------------------------------------------------------------------------

-- | This can be used to hold two LensLike functors.
-- The inner LensLike functor can be extracted from a @LensLike (PairMaybeFunctor f g) s t a b@
-- using 'fstLensLike' or 'sndLensLike'.
-- NB. The constructor must not be exported to keep 'fstLensLike' and 'sndLensLike' safe.
newtype PairMaybeFunctor f g a = PairMaybeFunctor { getPairMaybeFunctor :: (Maybe (f a), Maybe (g a)) }

instance (Functor f, Functor g) => Functor (PairMaybeFunctor f g) where
  fmap f (PairMaybeFunctor (a, b)) = PairMaybeFunctor (fmap f <$> a, fmap f <$> b)

instance (Apply f, Apply g) => Apply (PairMaybeFunctor f g) where
  (PairMaybeFunctor (a, b)) <.> (PairMaybeFunctor (c, d)) = PairMaybeFunctor (liftA2 (Data.Functor.Apply.<.>) a c, liftA2 (Data.Functor.Apply.<.>) b d)

instance (Applicative f, Applicative g) => Applicative (PairMaybeFunctor f g) where
  pure a = PairMaybeFunctor (Just $ pure a, Just $ pure a)
  (PairMaybeFunctor (a, b)) <*> (PairMaybeFunctor (c, d)) = PairMaybeFunctor (liftA2 (<*>) a c, liftA2 (<*>) b d)

instance (Contravariant f, Contravariant g) => Contravariant (PairMaybeFunctor f g) where
  contramap f (PairMaybeFunctor (a, b)) = PairMaybeFunctor (contramap f <$> a, contramap f <$> b)

fstLensLike :: LensLike (PairMaybeFunctor f g) s t a b -> LensLike f s t a b
-- fromJust is safe here as the constructor is hidden and we've definitely filled in the fst item of PairMaybeFunctor
fstLensLike l f b = fromJust . fst . getPairMaybeFunctor $ l (\a -> PairMaybeFunctor (Just $ f a, Nothing)) b

sndLensLike :: LensLike (PairMaybeFunctor f g) s t a b -> LensLike g s t a b
-- fromJust is safe here as the constructor is hidden and we've definitely filled in the snd item of PairMaybeFunctor
sndLensLike l f b = fromJust . snd . getPairMaybeFunctor $ l (\a -> PairMaybeFunctor (Nothing, Just $ f a)) b
