{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Strict
    ( Gadget(..)
    , toGadget
    , Widget(..)
    , HasWindow(..)
    , HasGadget(..)
    , statically
    , dynamically
    ) where

import Control.Applicative
import Control.Lens
import qualified Control.Lens.Internal.Zoom as Z
import Control.Monad.Reader
import Control.Monad.RWS.CPS hiding ((<>))
import Data.Biapplicative
import Data.Functor.Apply
import Data.Maybe
import Data.Profunctor.Unsafe as U
import Data.Semigroup
import Glazier

-- | The Elm update function is @a -> s -> (s, c)@
-- This is isomorphic to @ReaderT a (State s) c@
-- ie, given an action "a", and a current state "s", return the new state "s"
-- and any commands "c" that need to be interpreted externally (eg. download file).
-- This is named Gadget instead of Update to avoid confusion with update from Data.Map
-- This is also further enhanced with monadic and Writer effect, so we can just use RWST to avoid
-- writing new code.
newtype Gadget a w s m c = Gadget { runGadget :: RWST a w s m c }
  deriving (MonadState s, MonadWriter w, MonadReader a, Monad, Applicative, Functor)

makeWrapped ''Gadget

toGadget :: (Monad m, Monoid w) => (a -> s -> m (c, s, w)) -> Gadget a w s m c
toGadget f = Gadget $ do
      a <- ask
      s <- get
      (c, s', w) <- lift $ f a s
      put s'
      tell w
      pure c

instance (Monad m, Semigroup c) => Semigroup (Gadget a w s m c) where
   (Gadget f) <> (Gadget g) = Gadget $ (<>) <$> f <*> g

instance (Monad m, Monoid c) => Monoid (Gadget a w s m c) where
  mempty = Gadget $ pure mempty
  (Gadget f) `mappend` (Gadget g) = Gadget $ mappend <$> f <*> g

-- | zoom can be used to modify the state inside an Gadget
-- if CPS RWST has an instance of Zoom then we can just write:
-- type instance Zoomed (Gadget a w s m) = Zoomed (RWST a w s m)
-- type instance Zoomed (Gadget a w s m) = Z.FocusingWith w m
-- instance (Monad m, Monoid w) => Zoom (Gadget a w s m) (Gadget a w t m) s t where
--   zoom l = Gadget . zoom l . getGadget
--   {-# INLINE zoom #-}
type instance Zoomed (Gadget r w s m) = Z.FocusingWith w m
instance  (Monoid w, Monad m) => Zoom (Gadget r w s m) (Gadget r w t m) s t where
  zoom l (Gadget m) = toGadget $ \a s -> (Z.unfocusingWith U.#. l (Z.FocusingWith U.#. (runRWST m a))) s
  {-# INLINE zoom #-}

-- -- | magnify can be used to modify the action inside an Gadget
-- if CPS RWST has an instance of Magnify then we can just write:
-- type instance Magnified (Gadget a w s m) = Magnified (RWST a w s m)
-- instance (Monad m, Monoid w) => Magnify (Gadget a w s m) (Gadget b w s m) a b where
--   magnify l = Gadget . magnify l . getGadget
--   {-# INLINE magnify #-}
type instance Magnified (Gadget a w s m) = Z.EffectRWS w s m
instance (Monad m, Monoid w) => Magnify (Gadget b w s m) (Gadget a w s m) b a where
  magnify l (Gadget m) = toGadget $ \a s -> (Z.getEffectRWS U.#. l (Z.EffectRWS U.#. (runRWST m))) a s
  {-# INLINE magnify #-}

type instance Implanted (Gadget a w s m c) = Zoomed (Gadget a w s m) c
instance (Monad m, Monoid w) => Implant (Gadget a w s m c) (Gadget a w t m c) s t where
  implant = zoom

type instance Dispatched (Gadget a w s m c) = Magnified (Gadget a w s m) c
instance (Monad m, Monoid w) => Dispatch (Gadget a w s m c) (Gadget b w s m c) a b where
  dispatch = magnify

-------------------------------------------------------------------------------

-- | A widget is basically a tuple with Gadget and Window.
data Widget a w s m c v = Widget
  { widgetGadget :: Gadget a w s m c
  , widgetWindow :: Window s m v
  }

makeFields ''Widget

instance (Monad m, Semigroup c, Semigroup v) => Semigroup (Widget a w s m c v) where
  w1 <> w2 = Widget
    (widgetGadget w1 <> widgetGadget w2)
    (widgetWindow w1 <> widgetWindow w2)

instance (Monad m, Monoid c, Monoid v) => Monoid (Widget a w s m c v) where
  mempty = Widget mempty mempty
  mappend w1 w2 = Widget
    (widgetGadget w1 `mappend` widgetGadget w2)
    (widgetWindow w1 `mappend` widgetWindow w2)

instance Functor m => Bifunctor (Widget a w s m) where
  bimap f g w = Widget
    (f <$> widgetGadget w)
    (g <$> widgetWindow w)

instance (Monad m) => Biapplicative (Widget a w s m) where
  bipure a b = Widget
    (pure a)
    (pure b)
  f <<*>> a = Widget
    (widgetGadget f <*> widgetGadget a)
    (widgetWindow f <*> widgetWindow a)

statically :: (Monad m, Monoid c) => Window s m v -> Widget a w s m c v
statically = Widget mempty

dynamically :: (Monad m, Monoid v) => Gadget a w s m c -> Widget a w s m c v
dynamically v = Widget v mempty

type instance Dispatched (Widget a w s m c v) = Dispatched (Gadget a w s m c)
instance (Monad m, Monoid w) => Dispatch (Widget a w s m c v) (Widget b w s m c v) a b where
  dispatch p w = Widget
    (dispatch p $ widgetGadget w)
    (widgetWindow w)

type instance Implanted (Widget a w s m c v) = PairMaybeFunctor (Implanted (Gadget a w s m c)) (Implanted (Window s m v))
instance (Monad m, Monoid w) => Implant (Widget a w s m c v) (Widget a w t m c v) s t where
  implant l w = Widget
    (implant (fstLensLike l) $ widgetGadget w)
    (implant (sndLensLike l) $ widgetWindow w)

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
