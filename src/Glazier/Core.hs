{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
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
    , depict
    , Notify(..)
    , notify
    , mkNotifyR
    , mkNotifyRS
    , mkNotifyRS'
    , Widget(..)
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
import Control.Monad.State.Strict
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
-- Therefore, using the fundamental type of @view :: model -> html@, this is
-- isomorphic to @(->) s @ Reader, whose instances of Functor, Applicative
-- and Monad can be used to change the render type.
-- This is named Depict instead of View to avoid confusion with view from Control.Lens
newtype Depict s d = Depict { getDepict :: s -> d }
  deriving (MonadReader s, Monad, Applicative, Functor, Semigroup, Monoid)

makeWrapped ''Depict

-------------------------------------------------------------------------------

-- | The Elm update function is @a -> s -> (s, c)@
-- This is isomorphic to @ReaderT a (State s) c@
-- ie, given an action "a", and a current state "s", return the new state "s"
-- and any commands "c" that need to be interpreted externally (eg. download file).
-- This is named Notify instead of Update to avoid confusion with update from Data.Map
newtype Notify a s c = Notify { getNotify :: ReaderT a (State s) c }
  deriving (MonadState s, MonadReader a, Monad, Applicative, Functor)

makeWrapped ''Notify

instance Semigroup c => Semigroup (Notify a s c) where
   (Notify f) <> (Notify g) = Notify $ do
    a <- ask
    lift $ (<>) <$> runReaderT f a <*> runReaderT g a

instance Monoid c => Monoid (Notify a s c) where
  mempty = Notify $ lift $ state $ (,) mempty
  (Notify f) `mappend` (Notify g) = Notify $ do
    a <- ask
    lift $ mappend <$> runReaderT f a <*> runReaderT g a

-- | Useful for converting functions eventually to 'Notify'.
mkNotifyR :: (a -> State s c) -> Notify a s c
mkNotifyR = Notify . ReaderT

-- | Useful for converting functions eventually to 'Notify'.
mkNotifyRS :: (a -> s -> (c, s)) -> Notify a s c
mkNotifyRS = Notify . ReaderT . (state .)

-- | Useful for converting functions eventually to 'Notify'.
mkNotifyRS' :: Monoid c => (a -> s -> s) -> Notify a s c
mkNotifyRS' = Notify . ReaderT . (state' .)
 where
   state' f = state $ (,) mempty <$> f -- opposite of execState

-------------------------------------------------------------------------------

-- | A widget is basically a tuple with Notify and Depict.
data Widget a s c d = Widget
  { widgetNotify :: Notify a s c -- a -> s -> (s, c)
  , widgetDepict :: Depict s d -- s -> d
  }

makeFields ''Widget

instance (Semigroup c, Semigroup d) => Semigroup (Widget a s c d) where
  w1 <> w2 = Widget
    (widgetNotify w1 <> widgetNotify w2)
    (widgetDepict w1 <> widgetDepict w2)

instance (Monoid c, Monoid d) => Monoid (Widget a s c d) where
  mempty = Widget mempty mempty
  mappend w1 w2 = Widget
    (widgetNotify w1 `mappend` widgetNotify w2)
    (widgetDepict w1 `mappend` widgetDepict w2)

instance Bifunctor (Widget a s) where
  bimap f g w = Widget
    (f <$> widgetNotify w)
    (g <$> widgetDepict w)

instance Biapplicative (Widget a s) where
  bipure a b = Widget
    (pure a)
    (pure b)
  f <<*>> a = Widget
    (widgetNotify f <*> widgetNotify a)
    (widgetDepict f <*> widgetDepict a)

statically :: Monoid c => Depict s d -> Widget a s c d
statically = Widget mempty

dynamically :: Monoid d => Notify a s c -> Widget a s c d
dynamically v = Widget v mempty

-------------------------------------------------------------------------------

-- | magnify can be used to modify the action inside an Notify
type instance Magnified (Notify a s) = Effect (State s)
instance Magnify (Notify a s) (Notify b s) a b where
  magnify l = Notify . magnify l . getNotify
  {-# INLINE magnify #-}

-- | zoom can be used to modify the state inside an Notify
type instance Zoomed (Notify a s) = Zoomed (State s)
instance Zoom (Notify a s) (Notify a t) s t where
  zoom l = Notify . zoom l . getNotify
  {-# INLINE zoom #-}

-------------------------------------------------------------------------------

-- | Modify the state given a lens, prism or traversal.
-- NB. This is 'Control.Lens.Zoom' for Notify.
type family Implanted m :: * -> *
class Implant m n s t | m -> s, n -> t, m t -> n, n s -> m where
  implant :: LensLike' (Implanted m) t s -> m -> n

type instance Implanted (Depict s d) = Const d
instance Implant (Depict s d) (Depict t d) s t where
  implant l = Depict . magnify l . getDepict

type instance Implanted (Notify a s c) = Zoomed (Notify a s) c
instance Implant (Notify a s c) (Notify a t c) s t where
  implant = zoom

type instance Implanted (Widget a s c d) = PairMaybeFunctor (Implanted (Notify a s c)) (Implanted (Depict s d))
instance Implant (Widget a s c d) (Widget a t c d) s t where
  implant l w = Widget
    (implant (fstLensLike l) $ widgetNotify w)
    (implant (sndLensLike l) $ widgetDepict w)

-------------------------------------------------------------------------------

-- | Changes the action type given a prism
class Dispatch m n a b | m -> a, n -> b, m b -> n, n a -> m where
  dispatch :: Prism' b a -> m -> n

instance Monoid c => Dispatch (Notify a s c) (Notify b s c) a b where
  dispatch = magnify

instance Monoid c => Dispatch (Widget a s c v) (Widget b s c v) a b where
  dispatch p w = Widget
    (dispatch p $ widgetNotify w)
    (widgetDepict w)

-------------------------------------------------------------------------------

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
