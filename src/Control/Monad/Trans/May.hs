{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.May
   ( MayT(..)
   , May
   ) where

import Control.Applicative
import Control.Lens
import qualified Control.Lens.Internal.Zoom as IZ
import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.RWS.Class
import Control.Monad.Zip
import Data.Functor.Classes
import Data.Maybe
import Data.Profunctor.Unsafe
import Data.Semigroup

-- | Alternative version of MayT which has Semigroup instance that uses the return type Semigroup
-- and a Monoid mempty that returns Nothing.
newtype MayT m a = MayT { runMayT :: m (Maybe a) }

type May = MayT Identity

mapMayT :: (m (Maybe a) -> n (Maybe b)) -> MayT m a -> MayT n b
mapMayT f = MayT . f . runMayT

-- Based on https://hackage.haskell.org/package/transformers-0.5.4.0/docs/src/Control.Monad.Trans.Maybe.html#MayT

instance (Eq1 m) => Eq1 (MayT m) where
    liftEq eq (MayT x) (MayT y) = liftEq (liftEq eq) x y

instance (Ord1 m) => Ord1 (MayT m) where
    liftCompare comp (MayT x) (MayT y) = liftCompare (liftCompare comp) x y

instance (Read1 m) => Read1 (MayT m) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "MayT" MayT
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance (Show1 m) => Show1 (MayT m) where
    liftShowsPrec sp sl d (MayT m) =
        showsUnaryWith (liftShowsPrec sp' sl') "MayT" d m
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance (Eq1 m, Eq a) => Eq (MayT m a) where (==) = eq1
instance (Ord1 m, Ord a) => Ord (MayT m a) where compare = compare1
instance (Read1 m, Read a) => Read (MayT m a) where readsPrec = readsPrec1
instance (Show1 m, Show a) => Show (MayT m a) where showsPrec = showsPrec1

instance MonadTrans MayT where
    lift = MayT . liftA Just

instance (Functor m) => Functor (MayT m) where
    fmap f (MayT m) = MayT (fmap f <$> m)


instance (Functor m, Monad m) => Applicative (MayT m) where
    pure = lift . pure

    mf <*> mx = MayT $ do
        mb_f <- runMayT mf
        case mb_f of
            Nothing -> pure Nothing
            Just f  -> do
                mb_x <- runMayT mx
                case mb_x of
                    Nothing -> pure Nothing
                    Just x  -> pure (Just (f x))

instance (Functor m, Monad m) => Alternative (MayT m) where
    empty = MayT (pure Nothing)

    x <|> y = MayT $ do
        v <- runMayT x
        case v of
            Nothing -> runMayT y
            Just _  -> pure v

instance (Monad m) => Monad (MayT m) where
    x >>= f = MayT $ do
        v <- runMayT x
        case v of
            Nothing -> pure Nothing
            Just y  -> runMayT (f y)

    fail _ = MayT (pure Nothing)

instance (Monad m) => Fail.MonadFail (MayT m) where
    fail _ = MayT (pure Nothing)

instance (Monad m) => MonadPlus (MayT m) where
    mzero = MayT (pure Nothing)

    mplus x y = MayT $ do
        v <- runMayT x
        case v of
            Nothing -> runMayT y
            Just _  -> pure v

instance (Foldable f) => Foldable (MayT f) where
    foldMap f (MayT a) = foldMap (foldMap f) a

instance (Traversable f) => Traversable (MayT f) where
    traverse f (MayT a) = MayT <$> traverse (traverse f) a

instance (MonadZip m) => MonadZip (MayT m) where
    mzipWith f (MayT a) (MayT b) = MayT $ mzipWith (liftA2 f) a b

instance (MonadFix m) => MonadFix (MayT m) where
    mfix f = MayT (mfix (runMayT . f . fromMaybe bomb))
      where bomb = error "mfix (MayT): inner computation returned Nothing"

instance (MonadIO m) => MonadIO (MayT m) where
    liftIO = lift . liftIO
-- ---------------------------------------------------------------------------
-- Based on https://hackage.haskell.org/package/mtl-2.2.1
-- All of these instances need UndecidableInstances,

instance MonadCont m => MonadCont (MayT m) where
    callCC = liftCallCC' callCC
      where
        liftCallCC' callCC' f =
            MayT $ callCC' $ \ c -> runMayT (f (MayT . c . Just))

instance MonadError e m => MonadError e (MayT m) where
    throwError = lift . throwError
    catchError = liftCatch' catchError
      where
        liftCatch' f m h = MayT $ f (runMayT m) (runMayT . h)

instance MonadReader r m => MonadReader r (MayT m) where
    ask   = lift ask
    local f (MayT m) = MayT (local f m)
    reader = lift . reader

instance MonadState s m => MonadState s (MayT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadWriter w m => MonadWriter w (MayT m) where
    writer = lift . writer
    tell   = lift . tell
    listen = liftListen' listen
      where
        liftListen' listen' = mapMayT $ \ m -> do
            (a, w) <- listen' m
            pure $! fmap (\ r -> (r, w)) a
    pass = liftPass' pass
      where
        liftPass' pass' = mapMayT $ \ m -> pass' $ do
            a <- m
            pure $! case a of
                Nothing     -> (Nothing, id)
                Just (v, f) -> (Just v, f)

instance MonadRWS r w s m => MonadRWS r w s (MayT m)

-- Based on https://hackage.haskell.org/package/mmorph-1.1.0/docs/src/Control-Monad-Morph.html

instance MFunctor MayT where
    hoist nat m = MayT (nat (runMayT m))

instance MMonad MayT where
    embed f m = MayT $ do
        x <- runMayT (f (runMayT m))
        pure $ case x of
            Nothing       -> Nothing
            Just Nothing  -> Nothing
            Just (Just a) -> Just a

-- Lens
makeWrapped ''MayT

-- https://hackage.haskell.org/package/lens-4.15.4/docs/src/Control.Lens.Zoom.html

type instance Zoomed (MayT m) = IZ.FocusingMay (Zoomed m)
instance Zoom m n s t => Zoom (MayT m) (MayT n) s t where
  zoom l = MayT . fmap IZ.getMay . zoom (\afb -> IZ.unfocusingMay #. l (IZ.FocusingMay #. afb)) . fmap IZ.May . runMayT

-- New behavour starts here
instance (Applicative m, Semigroup r) => Semigroup (MayT m r) where
    (MayT m) <> (MayT m') = MayT (liftA2 (<>) m m')

instance (Applicative m, Semigroup r) => Monoid (MayT m r) where
    mempty =  MayT (pure Nothing)
    mappend = (<>)
