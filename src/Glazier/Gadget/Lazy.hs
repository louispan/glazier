{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Gadget.Lazy where

import Control.Applicative
import Control.Arrow
import qualified Control.Category as C
import Control.Lens
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix (MonadFix)
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Profunctor
import Data.Semigroup
import Glazier.Class

-- | The Elm update function is @a -> s -> (s, c)@
-- This is isomorphic to @ReaderT a (State s) c@
-- ie, given an action "a", and a current state "s", return the new state "s"
-- and any commands "c" that need to be interpreted externally (eg. download file).
-- This is named Gadget instead of Update to avoid confusion with update from Data.Map
newtype Gadget s m a c = Gadget
    { runGadget :: ReaderT a (StateT s m) c
    } deriving ( MonadState s
               , MonadReader a
               , Monad
               , Applicative
               , Functor
               , Fail.MonadFail
               , Alternative
               , MonadPlus
               , MonadFix
               , MonadIO
               )

class HasGadget s a | s -> a where
  gadget :: Lens' s a

instance HasGadget (Gadget s m a c) (Gadget s m a c) where
  gadget = id

makeWrapped ''Gadget

-- | NB lift can be simulated:
-- liftGadget :: (MonadTrans t, Monad m) => Gadget s m a c -> Gadget s (t m) a c
-- liftGadget = _Wrapping Gadget %~ hoist (hoist lift)
hoistGadget :: (Monad m) => (forall b. m b -> n b) -> Gadget s m a c -> Gadget s n a c
hoistGadget g = _Wrapping Gadget %~ hoist (hoist g)
{-# INLINABLE hoistGadget #-}

-- | This Iso gives the following functions:
--
-- @
-- underGadget :: (ReaderT a (StateT s m) c -> ReaderT a' (StateT s' m') c') -> Gadget s m a c -> Gadget s' m' a' c'
-- underGadget f = _Wrapping Gadget %~ f
--
-- overGadget :: (Gadget s m a c -> Gadget s' m' a' c') -> ReaderT a (StateT s m) c -> ReaderT a' (StateT s' m') c'
-- overGadget f = _Unwrapping Gadget %~ f
--
-- belowGadget :: (a -> s -> m (c, s)) (a' -> s' -> m' (c', s')) -> Gadget s m a c -> Gadget s' m' a' c'
-- belowGadget f = _Gadget %~ f
--
-- aboveGadget :: (Gadget s m a c -> Gadget s' m' a' c') -> (a -> s -> m (c, s)) (a' -> s' -> m' (c', s'))
-- aboveGadget f = from _Gadget %~ f
--
-- mkGadget' :: (a -> s -> m (c, s)) -> Gadget s m a c
-- mkGadget' = review _Gadget
--
-- runGadget' :: Gadget s m a c -> (a -> s -> m (c, s))
-- runGadget' = view _Gadget
-- @
--
_Gadget :: Iso (Gadget s m a c) (Gadget s' m' a' c') (a -> s -> m (c, s)) (a' -> s' -> m' (c', s'))
_Gadget = _Wrapping Gadget . iso runReaderT ReaderT . iso (runStateT .) (StateT .)
{-# INLINABLE _Gadget #-}

-- | Non polymorphic version of _Gadget
_Gadget' :: Iso' (Gadget s m a c) (a -> s -> m (c, s))
_Gadget' = _Gadget
{-# INLINABLE _Gadget' #-}

instance (Monad m, Semigroup c) => Semigroup (Gadget s m a c) where
    (Gadget f) <> (Gadget g) = Gadget $ (<>) <$> f <*> g
    {-# INLINABLE (<>) #-}

instance (Monad m, Monoid c) => Monoid (Gadget s m a c) where
    mempty = Gadget $ pure mempty
    {-# INLINABLE mempty #-}

    (Gadget f) `mappend` (Gadget g) = Gadget $ mappend <$> f <*> g
    {-# INLINABLE mappend #-}

instance Monad m => Profunctor (Gadget s m) where
    dimap f g (Gadget (ReaderT m)) = Gadget $ ReaderT $ \a -> StateT $ \s -> undefined
        (first g) <$> runStateT (m (f a)) s
    {-# INLINABLE dimap #-}

instance Monad m => Strong (Gadget s m) where
    first' (Gadget (ReaderT bc)) = Gadget $ ReaderT $ \(b, d) -> StateT $ \s ->
        (\(c, s') -> ((c, d), s')) <$> runStateT (bc b) s
    {-# INLINABLE first' #-}

instance Monad m => C.Category (Gadget s m) where
    id = Gadget $ ReaderT $ \a -> StateT $ \s -> pure (a, s)
    {-# INLINABLE id #-}

    Gadget (ReaderT bc) . Gadget (ReaderT ab) = Gadget $ ReaderT $ \a -> StateT $ \s -> do
        -- This line is the main difference between Strict and Lazy versions
        ~(b, s') <- runStateT (ab a) s
        runStateT (bc b) s'
    {-# INLINABLE (.) #-}

instance Monad m => Arrow (Gadget s m) where
    arr f = dimap f id C.id
    {-# INLINABLE arr #-}

    first = first'
    {-# INLINABLE first #-}

instance Monad m => Choice (Gadget s m) where
    left' (Gadget (ReaderT bc)) = Gadget $ ReaderT $ \db -> StateT $ \s -> case db of
        Left b -> do
            -- This line is the main difference between Strict and Lazy versions
            ~(c, s') <- runStateT (bc b) s
            pure (Left c, s')
        Right d -> pure (Right d, s)
    {-# INLINABLE left' #-}

instance Monad m => ArrowChoice (Gadget s m) where
    left = left'
    {-# INLINABLE left #-}

instance Monad m => ArrowApply (Gadget s m) where
    app = Gadget $ ReaderT $ \(Gadget (ReaderT bc), b) -> StateT $ \s -> runStateT (bc b) s
    {-# INLINABLE app #-}

instance MonadPlus m => ArrowZero (Gadget s m) where
    zeroArrow = Gadget mzero
    {-# INLINABLE zeroArrow #-}

instance MonadPlus m => ArrowPlus (Gadget s m) where
    Gadget a <+> Gadget b = Gadget (a `mplus` b)
    {-# INLINABLE (<+>) #-}

-- | zoom can be used to modify the state inside an Gadget
type instance Zoomed (Gadget s m a) = Zoomed (StateT s m)
instance Monad m => Zoom (Gadget s m a) (Gadget t m a) s t where
    zoom l = Gadget . zoom l . runGadget
    {-# INLINABLE zoom #-}

-- | magnify can be used to modify the action inside an Gadget
type instance Magnified (Gadget s m a) = Magnified (ReaderT a (StateT s m))
instance Monad m => Magnify (Gadget s m a) (Gadget s m b) a b where
    magnify l = Gadget . magnify l . runGadget
    {-# INLINABLE magnify #-}

type instance Implanted (Gadget s m a c) = Zoomed (Gadget s m a) c
instance Monad m => Implant (Gadget s m a c) (Gadget t m a c) s t where
    implant = zoom
    {-# INLINABLE implant #-}

type instance Dispatched (Gadget s m a c) = Magnified (Gadget s m a) c
instance Monad m => Dispatch (Gadget s m a c) (Gadget s m b c) a b where
    dispatch = magnify
    {-# INLINABLE dispatch #-}
