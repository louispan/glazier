{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Glazier.Widget
    ( Widget(..)
    , _window
    , _gadget
    , _window'
    , _gadget'
    , _Widget
    , _Widget'
    , _WrappingWidget
    , _WrappingWidget'
    , belowWidget
    , underWidget
    , overWidget
    , aboveWidget
    , mkWidget
    , mkWidget'
    , runWidget
    , runWidget'
    , statically
    , dynamically
    ) where

import Control.Applicative
import Control.Lens
import Data.Functor.Apply
import Data.Maybe
import Data.Semigroup
import Glazier.Class
import Glazier.Gadget
import Glazier.Window

-- | A widget is basically a tuple with Gadget and Window, but with handy instances for implant and dispatch.
data Widget m v a s n c = Widget
  { window :: WindowT s m v
  , gadget :: GadgetT a s n c
  }

-- | polymorphic lens to the window of a widget
_window :: Lens (Widget m v a s n c) (Widget m' v' a s n c) (WindowT s m v) (WindowT s m' v')
_window = lens window (\(Widget _ g) w -> Widget w g)
{-# INLINABLE _window #-}

-- | polymorphic lens to the gadget of a widget
_gadget :: Lens (Widget m v a s n c) (Widget m v a' s n' c') (GadgetT a s n c) (GadgetT a' s n' c')
_gadget = lens gadget (\(Widget w _) g -> Widget w g)
{-# INLINABLE _gadget #-}

-- | non polymorphic lens to the window of a widget
_window' :: Lens' (Widget m v a s n c) (WindowT s m v)
_window' = _window
{-# INLINABLE _window' #-}

-- | non polymorphic lens to the gadget of a widget
_gadget' :: Lens' (Widget m v a s n c) (GadgetT a s n c)
_gadget' = _gadget
{-# INLINABLE _gadget' #-}

_Widget :: Iso (Widget m v a s n c) (Widget m' v' a' s' n' c')
           (s -> m v, a -> s -> n (c, s)) (s' -> m' v', a' -> s' -> n' (c', s'))
_Widget = iso (\(Widget w g) -> (view _WindowT w, view _GadgetT g))
               (\(w, g) -> Widget (review _WindowT w) (review _GadgetT g))
{-# INLINABLE _Widget #-}

-- | Non polymorphic version of _Widget
_Widget' :: Iso' (Widget m v a s n c) (s -> m v, a -> s -> n (c, s))
_Widget' = _Widget
{-# INLINABLE _Widget' #-}

_WrappingWidget :: Iso (Widget m v a s n c) (Widget m' v' a' s' n' c')
           (WindowT s m v, GadgetT a s n c) (WindowT s' m' v', GadgetT a' s' n' c')
_WrappingWidget = iso (\(Widget w g) -> (w, g))
               (\(w, g) -> Widget w g)
{-# INLINABLE _WrappingWidget #-}

-- | Non polymorphic version of _WrappingWidget
_WrappingWidget' :: Iso' (Widget m v a s n c) (WindowT s m v, GadgetT a s n c)
_WrappingWidget' = _WrappingWidget
{-# INLINABLE _WrappingWidget' #-}

mkWidget :: (WindowT s m v, GadgetT a s n c) -> Widget m v a s n c
mkWidget = review _WrappingWidget
{-# INLINABLE mkWidget #-}

mkWidget' :: (s -> m v, a -> s -> n (c, s)) -> Widget m v a s n c
mkWidget' = review _Widget
{-# INLINABLE mkWidget' #-}

runWidget :: Widget m v a s n c -> (WindowT s m v, GadgetT a s n c)
runWidget = view _WrappingWidget
{-# INLINABLE runWidget #-}

runWidget' :: Widget m v a s n c -> (s -> m v, a -> s -> n (c, s))
runWidget' = view _Widget
{-# INLINABLE runWidget' #-}

belowWidget ::
  ((s -> m v, a -> s -> n (c, s))
   -> (s' -> m' v', a' -> s' -> n' (c', s')))
  -> Widget m v a s n c -> Widget m' v' a' s' n' c'
belowWidget f = _Widget %~ f
{-# INLINABLE belowWidget #-}

underWidget ::
  ((WindowT s m v, GadgetT a s n c)
   -> (WindowT s' m' v', GadgetT a' s' n' c'))
  -> Widget m v a s n c -> Widget m' v' a' s' n' c'
underWidget f = _WrappingWidget %~ f
{-# INLINABLE underWidget #-}

overWidget ::
  (Widget m v a s n c -> Widget m' v' a' s' n' c')
  -> (WindowT s m v, GadgetT a s n c)
  -> (WindowT s' m' v', GadgetT a' s' n' c')
overWidget f = from _WrappingWidget %~ f
{-# INLINABLE overWidget #-}

aboveWidget ::
  (Widget m v a s n c -> Widget m' v' a' s' n' c')
  -> (s -> m v, a -> s -> n (c, s))
  -> (s' -> m' v', a' -> s' -> n' (c', s'))
aboveWidget f = from _Widget %~ f
{-# INLINABLE aboveWidget #-}

instance (Applicative m, Monad n, Semigroup v, Semigroup c) => Semigroup (Widget m v a s n c) where
    w1 <> w2 = Widget
      (window w1 <> window w2)
      (gadget w1 <> gadget w2)
    {-# INLINABLE (<>) #-}

instance (Applicative m, Monad n, Monoid v, Monoid c) => Monoid (Widget m v a s n c) where
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
instance Functor n => Functor (Widget m v a s n) where
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
instance (Applicative m, Monad n, Monoid v) => Applicative (Widget m v a s n) where
    pure c = Widget mempty (pure c)
    {-# INLINABLE pure #-}

    (Widget w1 fg) <*> (Widget w2 g) = Widget (w1 `mappend` w2) (fg <*> g)
    {-# INLINABLE (<*>) #-}

statically :: (Monad n, Monoid c) => WindowT s m v -> Widget m v a s n c
statically w = Widget w mempty
{-# INLINABLE statically #-}

dynamically :: (Applicative m, Monoid v) => GadgetT a s n c -> Widget m v a s n c
dynamically = Widget mempty
{-# INLINABLE dynamically #-}

type instance Dispatched (Widget m v a s n c) = Dispatched (GadgetT a s n c)
instance Monad n => Dispatch (Widget m v a s n c) (Widget m v b s n c) a b where
    dispatch p w = Widget
        (window w)
        (dispatch p $ gadget w)
    {-# INLINABLE dispatch #-}

type instance Implanted (Widget m v a s n c) =
     PairMaybeFunctor (Implanted (WindowT s m v))
       (Implanted (GadgetT a s n c))
instance (Monad m, Monad n) => Implant (Widget m v a s n c) (Widget m v a t n c) s t where
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
