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
    , _Widget
    , _Widget'
    , _WrappingWidget
    , _WrappingWidget'
    , hoistWidget
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

-- | A widget is basically a tuple with Gadget and Window.
data Widget s v m a c = Widget
  { widgetWindow :: Window m s v
  , widgetGadget :: Gadget s m a c
  }

makeFields ''Widget

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
_Widget :: Iso (Widget s v m a c) (Widget s' v' m' a' c')
           (s -> m v, a -> s -> m (c, s)) (s' -> m' v', a' -> s' -> m' (c', s'))
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
_WrappingWidget :: Iso (Widget s v m a c) (Widget s' v' m' a' c')
           (Window m s v, Gadget s m a c) (Window m' s' v', Gadget s' m' a' c')
_WrappingWidget = iso (\(Widget w g) -> (w, g))
               (\(w, g) -> Widget w g)
{-# INLINABLE _WrappingWidget #-}

-- | Non polymorphic version of _WrappingWidget
_WrappingWidget' :: Iso' (Widget s v m a c) (Window m s v, Gadget s m a c)
_WrappingWidget' = _WrappingWidget
{-# INLINABLE _WrappingWidget' #-}

-- | Non polymorphic version of _Widget
_Widget' :: Iso' (Widget s v m a c) (s -> m v, a -> s -> m (c, s))
_Widget' = _Widget
{-# INLINABLE _Widget' #-}

-- | NB lift can be simulated:
-- liftWidget :: (MonadTrans t, Monad m) => Widget s v m a c -> Widget s v (t m) a c
-- liftWidget = hoistWidget lift
hoistWidget :: (Monad m) => (forall x. m x -> n x) -> Widget s v m a c -> Widget s v n a c
-- hoistWidget f (Widget w g) = Widget (hoistWindow f w) (hoistGadget f g)
hoistWidget f = _WrappingWidget %~ \(w, g) -> (hoistWindow f w, hoistGadget f g)
{-# INLINABLE hoistWidget #-}

instance (Monad m, Semigroup c, Semigroup v) => Semigroup (Widget s v m a c) where
    w1 <> w2 = Widget
      (widgetWindow w1 <> widgetWindow w2)
      (widgetGadget w1 <> widgetGadget w2)
    {-# INLINABLE (<>) #-}

instance (Monad m, Monoid c, Monoid v) => Monoid (Widget s v m a c) where
    mempty = Widget mempty mempty
    {-# INLINABLE mempty #-}

    mappend w1 w2 = Widget
        (widgetWindow w1 `mappend` widgetWindow w2)
        (widgetGadget w1 `mappend` widgetGadget w2)
    {-# INLINABLE mappend #-}

-- | Widget Functor is lawful
-- 1: fmap id  =  id
-- (Widget w g) = Widget w (id <$> g) =  Widget w g
-- 2: fmap (f . g) = fmap f . fmap g
-- (Widget w gad) = Widget w ((f . g) <$> gad) = Widget w ((fmap f . fmap g) gad)
instance Functor m => Functor (Widget s v m a) where
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
instance (Semigroup v, Monad m, Monoid v) => Applicative (Widget s v m a) where
    pure c = Widget mempty (pure c)
    {-# INLINABLE pure #-}

    (Widget w1 fg) <*> (Widget w2 g) = Widget (w1 <> w2) (fg <*> g)
    {-# INLINABLE (<*>) #-}

instance Monad m => Profunctor (Widget s v m) where
    dimap f g (Widget w m) = Widget w (dimap f g m)
    {-# INLINABLE dimap #-}

instance Monad m => Strong (Widget s v m) where
    first' (Widget w g) = Widget w (first' g)
    {-# INLINABLE first' #-}

instance (Monad m, Monoid v) => C.Category (Widget s v m) where
    id = Widget mempty C.id
    {-# INLINABLE id #-}

    Widget wbc gbc . Widget wab gab = Widget
        (wab `mappend` wbc)
        (gbc C.. gab)
    {-# INLINABLE (.) #-}

-- | No monad instance for Widget is possible, however an arrow is possible.
-- The Arrow instance monoidally appends the Window, and uses the inner Gadget Arrow instance.
instance (Monad m, Monoid v) => Arrow (Widget s v m) where
    arr f = dimap f id C.id
    {-# INLINABLE arr #-}

    first = first'
    {-# INLINABLE first #-}

instance (Monad m) => Choice (Widget s v m) where
    left' (Widget w bc) = Widget w (left' bc)
    {-# INLINABLE left' #-}

instance (Monad m, Monoid v) => ArrowChoice (Widget s v m) where
    left = left'
    {-# INLINABLE left #-}

statically :: (Monad m, Monoid c) => Window m s v -> Widget s v m a c
statically w = Widget w mempty
{-# INLINABLE statically #-}

dynamically :: (Monad m, Monoid v) => Gadget s m a c -> Widget s v m a c
dynamically = Widget mempty
{-# INLINABLE dynamically #-}

type instance Dispatched (Widget s v m a c) = Dispatched (Gadget s m a c)
instance Monad m => Dispatch (Widget s v m a c) (Widget s v m b c) a b where
    dispatch p w = Widget
        (widgetWindow w)
        (dispatch p $ widgetGadget w)
    {-# INLINABLE dispatch #-}

type instance Implanted (Widget s v m a c) =
     PairMaybeFunctor (Implanted (Window m s v))
       (Implanted (Gadget s m a c))
instance Monad m => Implant (Widget s v m a c) (Widget t v m a c) s t where
    implant l w = Widget
        (implant (fstLensLike l) $ widgetWindow w)
        (implant (sndLensLike l) $ widgetGadget w)
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
