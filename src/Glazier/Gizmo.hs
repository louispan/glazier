{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Glazier.Gizmo where

import Control.Applicative
import Control.Lens
import Control.Lens.Internal.Zoom
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix (MonadFix)
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Profunctor.Unsafe
import Data.Semigroup
import Glazier.Internal

newtype GizmoT i m o = GizmoT
    { unGizmoT :: ReaderT i (MaybeT m) o
    } deriving ( MonadReader i
               , Functor
               , Applicative
               , Monad
               , Fail.MonadFail
               , Alternative
               , MonadPlus
               , MonadFix
               , MonadIO
               )

deriving instance MonadState s m => MonadState s (GizmoT i m)

makeWrapped ''GizmoT

type Gizmo i = GizmoT i Identity

_GizmoT :: Iso (GizmoT i m o) (GizmoT i' m' o')
                (ReaderT i (MaybeT m) o) (ReaderT i' (MaybeT m') o')
_GizmoT = _Wrapping GizmoT

_GizmoT' :: Iso' (GizmoT i m o) (ReaderT i (MaybeT m) o)
_GizmoT' = _GizmoT

_GRT :: Iso (GizmoT i m o) (GizmoT i' m' o')
            (i -> (MaybeT m) o) (i' -> (MaybeT m') o')
_GRT = _GizmoT . iso runReaderT ReaderT

_GRT' :: Iso' (GizmoT i m o) (i -> (MaybeT m) o)
_GRT' = _GRT

_GRMT :: Iso (GizmoT i m o) (GizmoT i' m' o')
            (i -> m (Maybe o)) (i' -> m' (Maybe o'))
_GRMT = _GizmoT . iso runReaderT ReaderT .  iso (runMaybeT .) (MaybeT .)

_GRMT' :: Iso' (GizmoT i m o) (i -> m (Maybe o))
_GRMT' = _GRMT

-- | Creates a Gizmo that always uses the provided input.
constantly :: i -> GizmoT i m o -> GizmoT j m o
constantly i = over _GRT (\f _ -> f i)

instance MonadTrans (GizmoT i) where
    lift = GizmoT . lift . lift

instance MFunctor (GizmoT i) where
    hoist f (GizmoT m) = GizmoT (hoist (hoist f) m)

instance (Monad m, Semigroup o) => Semigroup (GizmoT i m o) where
    (<>) = liftA2 (<>)

instance (Monad m, Monoid o) => Monoid (GizmoT i m o) where
    mempty = pure mempty

    mappend = liftA2 mappend

-- | magnify can be used to modify the input type inside an Gizmo
-- This requires UndecidableInstances but is safe because @m@
-- is smaller than @GizmoT i m@
-- This instance means magnifying with a Prism will result in 'empty' if the traversal fails.
-- Otherwise it will result in mappened results of the monoid inside the Maybe.
type instance Magnified (GizmoT i m) = EffectMay m
instance Monad m => Magnify (GizmoT i m) (GizmoT j m) i j where
    magnify l (GizmoT (ReaderT m)) =
        GizmoT . ReaderT . fmap MaybeT $ getEffectMay #. l (EffectMay #. (runMaybeT <$> m))

-- type instance Magnified (MaybeT (ReaderT b m)) = EffectMay m
-- instance Monad m => Magnify (MaybeT (ReaderT b m)) (MaybeT (ReaderT a m)) b a where
--     magnify l (MaybeT (ReaderT m)) = MaybeT (ReaderT (getEffectMay #. l (EffectMay #. m)))

-- | zoom can be used to modify the state inside a Gizmo
-- This requires UndecidableInstances but is safe because @FocusingMay (Zoomed m)@
-- is smaller than (GizmoT i m)
type instance Zoomed (GizmoT i m) = FocusingMay (Zoomed m)
instance Zoom m n s t => Zoom (GizmoT i m) (GizmoT i n) s t where
    zoom l = GizmoT . ReaderT . ((MaybeT . fmap getMay . zoom (\afb -> unfocusingMay #. l (FocusingMay #. afb)) . fmap May . runMaybeT) .) . runReaderT . unGizmoT
