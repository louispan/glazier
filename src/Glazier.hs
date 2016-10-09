{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Functional version of (Elm Update/View & startApp architecture) enabling composable widgets, and a FRP-like framework.
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
-- This module uses isomorphic implementations Update and View resulting in instances can be be composed together into larger Widgets.
-- Original inspiration from https://arianvp.me/lenses-and-prisms-for-modular-clientside-apps/
--
-- This framework provides three main combinators:
-- * Semigroup and Monoid instances for concatenating widgets.
-- * 'dispatch' is used to re-route the action type.
-- * 'implant' is used to modify the model type.
module Glazier
    ( View(..)
    , Update(..)
    , updateR
    , updateRS
    , updateRS'
    , runReaderM
    , runUpdate
    , Widget(..)
    , HasWidget(..)
    , statically
    , dynamically
    , startWidget
    , Implanted
    , Implant(..)
    , Dispatch(..)
    ) where

import Control.Applicative
import Control.Category
import Control.Lens
import Control.Lens.Internal.Zoom
import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
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
newtype View s v = View { getView :: s -> v }
  deriving (MonadReader s, Monad, Applicative, Functor, Semigroup, Monoid)

makeWrapped ''View

-------------------------------------------------------------------------------

-- | The Elm update function is @a -> s -> (s, c)@
-- This is isomorphic to @ReaderT a (State s) c@
-- ie, given an action "a", and a current state "s", return the new state "s"
-- and any commands "c" that need to be interpreted externally
-- (eg. download file).
newtype Update a s c = Update { getUpdate :: ReaderT a (State s) c }
  deriving (MonadState s, MonadReader a, Monad, Applicative, Functor)

makeWrapped ''Update

instance Semigroup c => Semigroup (Update a s c) where
   (Update f) <> (Update g) = Update $ do
    a <- ask
    lift $ (<>) <$> runReaderT f a <*> runReaderT g a

instance Monoid c => Monoid (Update a s c) where
  mempty = Update $ lift $ state $ (,) mempty
  (Update f) `mappend` (Update g) = Update $ do
    a <- ask
    lift $ mappend <$> runReaderT f a <*> runReaderT g a

-- | Useful for converting functions eventually to 'Update'.
updateR :: (a -> State s c) -> Update a s c
updateR = Update . ReaderT

-- | Useful for converting functions eventually to 'Update'.
updateRS :: (a -> s -> (c, s)) -> Update a s c
updateRS = Update . ReaderT . (state .)

-- | Useful for converting functions eventually to 'Update'.
updateRS' :: Monoid c => (a -> s -> s) -> Update a s c
updateRS' = Update . ReaderT . (state' .)
 where
   state' f = state $ (,) mempty <$> f -- opposite of execState

-- | This function combines two different monadic effects.
--
-- This can enable past-Dependence.
-- Elm has foldp : (a -> state -> state) -> state -> Signal a -> Signal state
-- This is equivalent to a creating a @StateT state (Signal m) ()@
--
-- 'runReaderM' is a more general form of @StateT state (Signal m) ()@ where
-- given a reader monad to transform "a" to "c" with effects, and an "as"
-- monad that produces "a"s with other effects, run the result of "as" through
-- the reader monad to produce "c"s with both effects.
-- @
-- runReaderM :: Monad m => Reader a (State s) c -> m a                      -> StateT s m c
-- runReaderM ::            Reader a (State s) c -> Signal STM a             -> StateT state (Signal STM) c
-- runReaderM ::            Reader a (State s) c -> Pipes.Concurrent.Input a -> StateT state Pipes.Concurrent.Input c
-- @
runReaderM :: (Monad m, Monad (t m), MonadTrans t, MFunctor t)
  => ReaderT a (t Identity) c -> m a -> t m c
runReaderM c as = do
  a <- lift as
  hoist generalize $ runReaderT c a

-- | Runs one tick of the update processing, given handler functions, and stores
-- the updated state of the model.
-- * onTick is tick procesing function that returns commands,
--   rendering frame, and updated model state.
-- * onFrame is given the rendering frame.
-- * interpretCommands is given the commands.
-- An example of getTick function using Pipes.Concurrent is:
-- @
--  import Pipes.Concurrent
--
--  run = do
--    (address, signal, seal) <- PC.spawn' PC.unbounded
--    let mkCtl = PC.send address
--      xs = G.startWidget (appWidget mkCtl) signal
--      getTick s' = PC.recv (runStateT xs s')
-- @
runUpdate ::
  (Monad m, MonadState s m, MonadError s m) =>
  (frame -> m ())
  -> (cmds -> MaybeT m ())
  -> (s -> MaybeT m ((cmds, frame), s))
  -> m s
runUpdate
  onFrame
  interpretCommands
  getTick
  = do
  s' <- get
  mr <- runMaybeT $ do
    ((cmds, frame'), s'') <- getTick s'
    put s''
    -- NB cmds are processed before rendering, so that we can quit without rendering again
    interpretCommands cmds
    lift $ onFrame frame'
  -- on error, return the latest state not the initial state
  s'' <- get
  maybe (throwError s'') (const $ pure s'') mr

-------------------------------------------------------------------------------

-- | A widget is basically a tuple with Update and View.
data Widget a s c v = Widget
  { _widgetUpdate :: Update a s c -- a -> s -> (s, c)
  , _widgetView   :: View s v -- s -> v
  }

makeClassy ''Widget

instance (Semigroup c, Semigroup v) => Semigroup (Widget a s c v) where
  w1 <> w2 = Widget
    (_widgetUpdate w1 <> _widgetUpdate w2)
    (_widgetView w1 <> _widgetView w2)

instance (Monoid c, Monoid v) => Monoid (Widget a s c v) where
  mempty = Widget mempty mempty
  mappend w1 w2 = Widget
    (_widgetUpdate w1 `mappend` _widgetUpdate w2)
    (_widgetView w1 `mappend` _widgetView w2)

instance Bifunctor (Widget a s) where
  bimap f g w = Widget
    (f <$> _widgetUpdate w)
    (g <$> _widgetView w)

instance Biapplicative (Widget a s) where
  bipure a b = Widget
    (pure a)
    (pure b)
  f <<*>> a = Widget
    (_widgetUpdate f <*> _widgetUpdate a)
    (_widgetView f <*> _widgetView a)

statically :: Monoid c => View s v -> Widget a s c v
statically = Widget mempty

dynamically :: Monoid v => Update a s c -> Widget a s c v
dynamically v = Widget v mempty

-------------------------------------------------------------------------------
-- | A more generic form of Elm start app.
-- This uses the input Widget and input monad to result in a @StateT s m@ that:
-- * gets actions from an input monad,
-- * run the actions through the widget update to store the updated model,
-- * runs the model through the widget view,
-- * returns commands and rendered views.
-- This can be used in the getTick argument to 'runUpdate'.
startWidget :: Monad m => Widget a s c v -> m a -> StateT s m (c, v)
startWidget w signal =
  let u = _widgetUpdate w
      v = _widgetView w
      rs = runReaderM (getUpdate u) signal
      -- add the transformed view to the state output
      -- read from inbox of actions, and apply to Update
      -- applyView :: Monad m => m (c, s) -> m ((c, v), s)
      applyView m = do
        (c, s) <- m
        pure ((c, getView v s), s)
  in mapStateT applyView rs

-------------------------------------------------------------------------------

-- | magnify can be used to modify the action inside an Update
type instance Magnified (Update a s) = Effect (State s)
instance Magnify (Update a s) (Update b s) a b where
  magnify l = Update . magnify l . getUpdate
  {-# INLINE magnify #-}

-- | zoom can be used to modify the state inside an Update
type instance Zoomed (Update a s) = Zoomed (State s)
instance Zoom (Update a s) (Update a t) s t where
  zoom l = Update . zoom l . getUpdate
  {-# INLINE zoom #-}

-------------------------------------------------------------------------------

-- | Modify the state given a lens, prism or traversal.
-- NB. This is 'Control.Lens.Zoom' for Update.
type family Implanted m :: * -> *
class Implant m n s t | m -> s, n -> t, m t -> n, n s -> m where
  implant :: LensLike' (Implanted m) t s -> m -> n

type instance Implanted (View s v) = Const v
instance Implant (View s v) (View t v) s t where
  implant l = View . magnify l . getView

type instance Implanted (Update a s c) = Zoomed (Update a s) c
instance Implant (Update a s c) (Update a t c) s t where
  implant = zoom

type instance Implanted (Widget a s c v) = PairMaybeFunctor (Implanted (Update a s c)) (Implanted (View s v))
instance Implant (Widget a s c v) (Widget a t c v) s t where
  implant l w = Widget
    (implant (fstLensLike l) $ _widgetUpdate w)
    (implant (sndLensLike l) $ _widgetView w)

-------------------------------------------------------------------------------

-- | Changes the action type given a prism
class Dispatch m n a b | m -> a, n -> b, m b -> n, n a -> m where
  dispatch :: Prism' b a -> m -> n

instance Monoid c => Dispatch (Update a s c) (Update b s c) a b where
  dispatch = magnify

instance Monoid c => Dispatch (Widget a s c v) (Widget b s c v) a b where
  dispatch p w = Widget
    (dispatch p $ _widgetUpdate w)
    (_widgetView w)

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
