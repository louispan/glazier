{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Window where

import Control.Applicative
import Control.Lens
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix (MonadFix)
import Control.Monad.Morph
import Control.Monad.Reader
import Data.Semigroup

-------------------------------------------------------------------------------

-- | The Elm view function is basically @view :: model -> html@
-- This can be enhanced with monadic effects with ReaderT.
-- This is named Window instead of View to avoid confusion with view from Control.Lens
newtype WindowT s m v = WindowT
    { getWindowT :: ReaderT s m v
    } deriving ( MonadReader s
               , Monad
               , Applicative
               , Functor
               , Fail.MonadFail
               , Alternative
               , MonadPlus
               , MonadFix
               , MonadIO
               )

makeWrapped ''WindowT

type Window s = WindowT s Identity

_WindowT :: Iso (WindowT s m v) (WindowT s' m' v') (s -> m v) (s' -> m' v')
_WindowT = _Wrapping WindowT . iso runReaderT ReaderT

-- | Non polymorphic version of _Window
_WindowT' :: Iso' (WindowT s m v) (s -> m v)
_WindowT' = _WindowT

mkWindowT :: (s -> m v) -> WindowT s m v
mkWindowT = review _WindowT

runWindowT :: WindowT s m v -> (s -> m v)
runWindowT = view _WindowT

belowWindowT ::
  ((s -> m v) -> (s' -> m' v'))
  -> WindowT s m v -> WindowT s' m' v'
belowWindowT f = _WindowT %~ f

underWindowT
    :: (ReaderT s m v -> ReaderT s' m' v')
    -> WindowT s m v
    -> WindowT s' m' v'
underWindowT f = _Wrapping WindowT %~ f

overWindowT
    :: (WindowT s m v -> WindowT s' m' v')
    -> ReaderT s m v
    -> ReaderT s' m' v'
overWindowT f = _Unwrapping WindowT %~ f

aboveWindowT ::
  (WindowT s m v -> WindowT s' m' v')
  -> (s -> m v) -> (s' -> m' v')
aboveWindowT f = from _WindowT %~ f

instance MonadTrans (WindowT s) where
    lift = WindowT . lift

instance MFunctor (WindowT s) where
    hoist f (WindowT m) = WindowT (hoist f m)

instance (Applicative m, Semigroup v) => Semigroup (WindowT s m v) where
    (<>) = liftA2 (<>)

instance (Applicative m, Monoid v) => Monoid (WindowT s m v) where
    mempty = pure mempty

    mappend = liftA2 mappend

-- | magnify can be used to modify the action inside an Gadget
-- This requires UndecidableInstances but is safe because (ReaderT s m)
-- is smaller than (WindowT s m)
type instance Magnified (WindowT s m) = Magnified (ReaderT s m)
instance Monad m => Magnify (WindowT s m) (WindowT t m) s t where
    magnify l = WindowT . magnify l . getWindowT
