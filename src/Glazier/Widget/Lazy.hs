{-# LANGUAGE MonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Widget.Lazy
    ( Widget(..)
    , _gadget
    , _window
    , _Widget
    , _Widget'
    , _WrappingWidget
    , _WrappingWidget'
    , statically
    , dynamically
    ) where

import Control.Applicative
import Control.Arrow
import qualified Control.Category as C
import Control.Lens
import Data.Functor.Apply
import Data.Maybe
import Data.Profunctor
import Data.Semigroup
import Glazier.Class
import Glazier.Gadget.Lazy
import Glazier.Window

-- | A widget is basically a tuple with Gadget and Window, but with handy instances for implant and dispatch.
data Widget m s v n a c = Widget
  { window :: Window m s v
  , gadget :: Gadget s n a c
  }

-- | polymorphic lens to the window of a widget
_window :: Lens (Widget m s v n a c) (Widget m' s v' n a c) (Window m s v) (Window m' s v')
_window = lens window (\(Widget _ g) w -> Widget w g)

-- | polymorphic lens to the gadget of a widget
_gadget :: Lens (Widget m s v n a c) (Widget m s v n' a' c') (Gadget s n a c) (Gadget s n' a' c')
_gadget = lens gadget (\(Widget w _) g -> Widget w g)

-- | non polymorphic lens to the window of a widget
_window' :: Lens' (Widget m s v n a c) (Window m s v)
_window' = _window

-- | non polymorphic lens to the gadget of a widget
_gadget' :: Lens' (Widget m s v n a c) (Gadget s n a c)
_gadget' = _gadget

-- | This Iso gives the following functions:
--
-- @
-- belowWidget :: ((s -> m v, a -> s -> m (c, s)) -> (s' -> m' v', a' -> s' -> m' (c', s'))) -> Widget s v m a c -> Widget s' v' m' a' c'
-- belowWidget f = _Widget %~ f
--
-- aboveWidget :: (Widget s v m a c -> Widget s' v' m' a' c') -> (s -> m v, a -> s -> m (c, s)) -> (s' -> m' v', a' -> s' -> m' (c', s'))
-- aboveWidget f = from _Widget %~ f
--
-- mkWidget' :: (s -> m v, a -> s -> m (c, s)) -> Widget s v m a c
-- mkWidget' = review _Widget
--
-- runWidget' :: Widget s v m a c -> (s -> m v, a -> s -> m (c, s))
-- runWidget' = view _Widget
-- @
--
_Widget :: Iso (Widget m s v n a c) (Widget m' s' v' n' a' c')
           (s -> m v, a -> s -> n (c, s)) (s' -> m' v', a' -> s' -> n' (c', s'))
_Widget = iso (\(Widget w g) -> (view _Window w, view _Gadget g))
               (\(w, g) -> Widget (review _Window w) (review _Gadget g))
{-# INLINABLE _Widget #-}

-- | This Iso gives the following functions:
--
-- @
-- underWidget :: ((Window m s v, Gadget s m a c) -> (Window m' s' v', Gadget s' m' a' c')) -> Widget s v m a c -> Widget s' v' m' a' c'
-- underWidget f = _WrappingWidget %~ f
--
-- overWidget :: (Widget s v m a c -> Widget s' v' m' a' c') -> (Window m s v, Gadget s m a c) -> (Window m' s' v', Gadget s' m' a' c')
-- overWidget f = from _WrappingWidget %~ f
--
-- mkWidget :: (Window m s v, Gadget s m a c) -> Widget s v m a c
-- mkWidget = review _WrappingWidget
--
-- runWidget :: Widget s v m a c -> (Window m s v, Gadget s m a c)
-- runWidget = view _WrappingWidget
-- @
--
_WrappingWidget :: Iso (Widget m s v n a c) (Widget m' s' v' n' a' c')
           (Window m s v, Gadget s n a c) (Window m' s' v', Gadget s' n' a' c')
_WrappingWidget = iso (\(Widget w g) -> (w, g))
               (\(w, g) -> Widget w g)
{-# INLINABLE _WrappingWidget #-}

-- | Non polymorphic version of _WrappingWidget
_WrappingWidget' :: Iso' (Widget m s v n a c) (Window m s v, Gadget s n a c)
_WrappingWidget' = _WrappingWidget
{-# INLINABLE _WrappingWidget' #-}

-- | Non polymorphic version of _Widget
_Widget' :: Iso' (Widget m s v n a c) (s -> m v, a -> s -> n (c, s))
_Widget' = _Widget
{-# INLINABLE _Widget' #-}

instance (Applicative m, Monad n, Semigroup c, Semigroup v) => Semigroup (Widget m s v n a c) where
    w1 <> w2 = Widget
      (window w1 <> window w2)
      (gadget w1 <> gadget w2)
    {-# INLINABLE (<>) #-}

instance (Applicative m, Monad n, Monoid c, Monoid v) => Monoid (Widget m s v n a c) where
    mempty = Widget mempty mempty
    {-# INLINABLE mempty #-}

    mappend w1 w2 = Widget
        (window w1 `mappend` window w2)
        (gadget w1 `mappend` gadget w2)
    {-# INLINABLE mappend #-}

-- | Widget Functor is lawful
-- 1: fmap id  =  id
-- (Widget w g) = Widget w (id <$> g) =  Widget w g
-- 2: fmap (f . g) = fmap f . fmap g
-- (Widget w gad) = Widget w ((f . g) <$> gad) = Widget w ((fmap f . fmap g) gad)
instance Functor n => Functor (Widget m s v n a) where
    fmap f (Widget w g) = Widget
        w
        (f <$> g)
    {-# INLINABLE fmap #-}

-- | Widget Applicative is lawful
-- Identity: pure id <*> v = v
-- Widget mempty (pure id) <*> Widget vw vg
--     = Widget (mempty <> vw) (pure id <*> vg)
--     = Widget vw vg
-- Composition: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- Widget mempty (pure (.)) <*> Widget uw ug <*> Widget vw vg <*> Widget ww wg =
--     = Widget (mempty <> uw <> vw <> ww) (pure (.) <*> ug <*> vg <*> wg
--     = Widget (uw <> vw <> ww) (ug <*> (vg <*> wg))
--     = Widget (uw <> (vw <> ww)) (ug <*> (vg <*> wg))
--     = Widget uw ug <*> (Widget vw vg <*> Widget ww wg)
-- Interchange: u <*> pure y = pure ($ y) <*> u
-- Widget uw ug <*> Widget mempty (pure y)
--     = Widget (uw <> mempty) (ug <*> pure y)
--     = Widget (mempty <> uw) (pure ($ y) <*> ug)
--     = Widget mempty (pure $y) <*> Widget uw ug
instance (Applicative m, Monad n, Semigroup v, Monoid v) => Applicative (Widget m s v n a) where
    pure c = Widget mempty (pure c)
    {-# INLINABLE pure #-}

    (Widget w1 fg) <*> (Widget w2 g) = Widget (w1 <> w2) (fg <*> g)
    {-# INLINABLE (<*>) #-}

instance (Applicative m, Monad n) => Profunctor (Widget m s v n) where
    dimap f g (Widget w m) = Widget w (dimap f g m)
    {-# INLINABLE dimap #-}

instance (Applicative m, Monad n) => Strong (Widget m s v n) where
    first' (Widget w g) = Widget w (first' g)
    {-# INLINABLE first' #-}

instance (Applicative m, Monad n, Monoid v) => C.Category (Widget m s v n) where
    id = Widget mempty C.id
    {-# INLINABLE id #-}

    Widget wbc gbc . Widget wab gab = Widget
        (wab `mappend` wbc)
        (gbc C.. gab)
    {-# INLINABLE (.) #-}

-- | No monad instance for Widget is possible, however an arrow is possible.
-- The Arrow instance monoidally appends the Window, and uses the inner Gadget Arrow instance.
instance (Applicative m, Monad n, Monoid v) => Arrow (Widget m s v n) where
    arr f = dimap f id C.id
    {-# INLINABLE arr #-}

    first = first'
    {-# INLINABLE first #-}

instance (Applicative m, Monad n) => Choice (Widget m s v n) where
    left' (Widget w bc) = Widget w (left' bc)
    {-# INLINABLE left' #-}

instance (Applicative m, Monad n, Monoid v) => ArrowChoice (Widget m s v n) where
    left = left'
    {-# INLINABLE left #-}

statically :: (Monad n, Monoid c) => Window m s v -> Widget m s v n a c
statically w = Widget w mempty
{-# INLINABLE statically #-}

dynamically :: (Applicative m, Monad n, Monoid v) => Gadget s n a c -> Widget m s v n a c
dynamically = Widget mempty
{-# INLINABLE dynamically #-}

type instance Dispatched (Widget m s v n a c) = Dispatched (Gadget s n a c)
instance Monad n => Dispatch (Widget m s v n a c) (Widget m s v n b c) a b where
    dispatch p w = Widget
        (window w)
        (dispatch p $ gadget w)
    {-# INLINABLE dispatch #-}

type instance Implanted (Widget m s v n a c) =
     PairMaybeFunctor (Implanted (Window m s v))
       (Implanted (Gadget s n a c))
instance (Monad m, Monad n) => Implant (Widget m s v n a c) (Widget m t v n a c) s t where
    implant l w = Widget
        (implant (fstLensLike l) $ window w)
        (implant (sndLensLike l) $ gadget w)
    {-# INLINABLE implant #-}

-- -------------------------------------------------------------------------------

-- | This can be used to hold two LensLike functors.
-- The inner LensLike functor can be extracted from a @LensLike (PairMaybeFunctor f g) s t a b@
-- using 'fstLensLike' or 'sndLensLike'.
-- NB. The constructor must not be exported to keep 'fstLensLike' and 'sndLensLike' safe.
newtype PairMaybeFunctor f g a = PairMaybeFunctor { getPairMaybeFunctor :: (Maybe (f a), Maybe (g a)) }

instance (Functor f, Functor g) => Functor (PairMaybeFunctor f g) where
    fmap f (PairMaybeFunctor (a, b)) = PairMaybeFunctor (fmap f <$> a, fmap f <$> b)
    {-# INLINABLE fmap #-}

instance (Apply f, Apply g) => Apply (PairMaybeFunctor f g) where
    (PairMaybeFunctor (a, b)) <.> (PairMaybeFunctor (c, d)) = PairMaybeFunctor (liftA2 (Data.Functor.Apply.<.>) a c, liftA2 (Data.Functor.Apply.<.>) b d)
    {-# INLINABLE (<.>) #-}

instance (Applicative f, Applicative g) => Applicative (PairMaybeFunctor f g) where
    pure a = PairMaybeFunctor (Just $ pure a, Just $ pure a)
    {-# INLINABLE pure #-}

    (PairMaybeFunctor (a, b)) <*> (PairMaybeFunctor (c, d)) = PairMaybeFunctor (liftA2 (<*>) a c, liftA2 (<*>) b d)
    {-# INLINABLE (<*>) #-}

instance (Contravariant f, Contravariant g) => Contravariant (PairMaybeFunctor f g) where
    contramap f (PairMaybeFunctor (a, b)) = PairMaybeFunctor (contramap f <$> a, contramap f <$> b)
    {-# INLINABLE contramap #-}

fstLensLike :: LensLike (PairMaybeFunctor f g) s t a b -> LensLike f s t a b
-- fromJust is safe here as the constructor is hidden and we've definitely filled in the fst item of PairMaybeFunctor
fstLensLike l f b = fromJust . fst . getPairMaybeFunctor $ l (\a -> PairMaybeFunctor (Just $ f a, Nothing)) b
{-# INLINABLE fstLensLike #-}

sndLensLike :: LensLike (PairMaybeFunctor f g) s t a b -> LensLike g s t a b
-- fromJust is safe here as the constructor is hidden and we've definitely filled in the snd item of PairMaybeFunctor
sndLensLike l f b = fromJust . snd . getPairMaybeFunctor $ l (\a -> PairMaybeFunctor (Nothing, Just $ f a)) b
{-# INLINABLE sndLensLike #-}
